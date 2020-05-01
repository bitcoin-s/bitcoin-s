package org.bitcoins.dlc.sign

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  DLCAcceptWithoutSigs,
  DLCMutualCloseSig,
  DLCOffer
}
import org.bitcoins.commons.jsonmodels.dlc.{
  CETSignatures,
  DLCPublicKeys,
  FundingSignatures
}
import org.bitcoins.core.crypto.ExtPrivateKey
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  P2WSHWitnessV0
}
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  Transaction,
  TransactionOutPoint
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto.{
  CryptoUtil,
  ECPrivateKey,
  ECPublicKey,
  FieldElement,
  SchnorrDigitalSignature,
  Sha256DigestBE
}
import org.bitcoins.dlc.builder.DLCTxBuilder

import scala.concurrent.{ExecutionContext, Future}

/** Responsible for constructing all DLC signatures
  * and signed transactions
  */
case class DLCTxSigner(
    builder: DLCTxBuilder,
    isInitiator: Boolean,
    extPrivKey: ExtPrivateKey,
    nextAddressIndex: Int,
    fundingUtxos: Vector[ScriptSignatureParams[InputInfo]])(implicit
    val ec: ExecutionContext) {
  private val offer = builder.offer
  private val accept = builder.accept

  private val fundingSPK = MultiSignatureScriptPubKey(
    2,
    Vector(offer.pubKeys.fundingKey, accept.pubKeys.fundingKey)
  )

  val fundingPrivKey: ECPrivateKey =
    extPrivKey
      .deriveChildPrivKey(BIP32Path.fromString(s"m/0/$nextAddressIndex"))
      .key

  val cetToLocalPrivKey: ECPrivateKey =
    extPrivKey
      .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${nextAddressIndex + 1}"))
      .key

  val finalPrivKey: ECPrivateKey =
    extPrivKey
      .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${nextAddressIndex + 2}"))
      .key

  private val pubKeys = DLCPublicKeys.fromExtPrivKeyAndIndex(extPrivKey,
                                                             nextAddressIndex,
                                                             builder.network)

  if (isInitiator) {
    require(
      pubKeys == offer.pubKeys,
      "Given ExtPrivateKey and index does not match the public keys in offer")
    require(fundingUtxos.map(_.outputReference) == offer.fundingInputs,
            "Funding ScriptSignatureParams did not match offer funding inputs")
  } else {
    require(
      pubKeys == accept.pubKeys,
      "Given ExtPrivateKey and index does not match the public keys in accept")
    require(
      fundingUtxos.map(_.outputReference) == accept.fundingInputs,
      s"Funding ScriptSignatureParams ($fundingUtxos) did not match accept funding inputs (${accept.fundingInputs})"
    )
  }

  /** Return's this party's payout for a given oracle signature */
  def getPayout(sig: SchnorrDigitalSignature): CurrencyUnit = {
    val (offerPayout, acceptPayout) = builder.getPayouts(sig)
    if (isInitiator) {
      offerPayout
    } else {
      acceptPayout
    }
  }

  /** Creates this party's FundingSignatures */
  def createFundingTxSigs(): Future[FundingSignatures] = {
    val sigFs =
      Vector.newBuilder[Future[(TransactionOutPoint, PartialSignature)]]

    for {
      fundingTx <- builder.buildFundingTx

      _ = {
        fundingUtxos.foreach { utxo =>
          utxo.toSingles.foreach { utxoSingle =>
            val sigF = BitcoinSigner.signSingle(utxoSingle,
                                                fundingTx,
                                                isDummySignature = false)
            sigFs += sigF.map((utxo.outPoint, _))
          }
        }
      }

      sigs <- Future.sequence(sigFs.result())
    } yield {
      val sigsMap = sigs.groupBy(_._1).map {
        case (outPoint, outPointAndSigs) =>
          outPoint -> outPointAndSigs.map(_._2)
      }

      FundingSignatures(sigsMap)
    }
  }

  /** Constructs the signed DLC funding transaction given remote FundingSignatures */
  def signFundingTx(remoteSigs: FundingSignatures): Future[Transaction] = {
    val fundingInputs = offer.fundingInputs ++ accept.fundingInputs

    val psbtF = for {
      localSigs <- createFundingTxSigs()
      allSigs = localSigs.merge(remoteSigs)
      fundingTx <- builder.buildFundingTx
    } yield {
      fundingInputs.zipWithIndex.foldLeft(PSBT.fromUnsignedTx(fundingTx)) {
        case (psbt, (OutputReference(outPoint, output), index)) =>
          val sigs = allSigs(outPoint)

          psbt
            .addWitnessUTXOToInput(output, index)
            .addSignatures(sigs, index)
      }
    }

    psbtF.flatMap { psbt =>
      val txT = psbt.finalizePSBT.flatMap(_.extractTransactionAndValidate)
      Future.fromTry(txT)
    }
  }

  private def findSigInPSBT(
      psbt: PSBT,
      pubKey: ECPublicKey): Future[PartialSignature] = {
    val sigOpt = psbt.inputMaps.head.partialSignatures
      .find(_.pubKey == pubKey)

    sigOpt match {
      case None =>
        Future.failed(new RuntimeException("No signature found after signing"))
      case Some(partialSig) => Future.successful(partialSig)
    }
  }

  private def createPartiallySignedMutualCloseTx(
      oracleSig: SchnorrDigitalSignature): Future[PSBT] = {
    for {
      fundingTx <- builder.buildFundingTx
      utx <- builder.buildMutualCloseTx(oracleSig)
      psbt =
        PSBT
          .fromUnsignedTx(utx)
          .addWitnessUTXOToInput(fundingTx.outputs.head, index = 0)
          .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
      signedPSBT <- psbt.sign(inputIndex = 0, fundingPrivKey)
    } yield {
      signedPSBT
    }
  }

  /** Constructs a DLCMutualCloseSig message given an oracle signature */
  def createMutualCloseTxSig(
      oracleSig: SchnorrDigitalSignature): Future[DLCMutualCloseSig] = {
    for {
      psbt <- createPartiallySignedMutualCloseTx(oracleSig)
      sig <- findSigInPSBT(psbt, fundingPrivKey.publicKey)
    } yield {
      DLCMutualCloseSig(accept.eventId, oracleSig, sig)
    }
  }

  /** Constructs a signed mutual close transaction given an oracle
    * signature and remote's mutual close sig (both found in a DLCMutualCloseSig) */
  def signMutualCloseTx(
      oracleSig: SchnorrDigitalSignature,
      remoteSig: PartialSignature): Future[Transaction] = {
    createPartiallySignedMutualCloseTx(oracleSig).flatMap { psbt =>
      val txT = psbt
        .addSignature(remoteSig, inputIndex = 0)
        .finalizePSBT
        .flatMap(_.extractTransactionAndValidate)

      Future.fromTry(txT)
    }
  }

  private def createPartiallySignedCET(
      msg: Sha256DigestBE,
      isOffer: Boolean): Future[PSBT] = {
    for {
      fundingTx <- builder.buildFundingTx
      utx <- {
        if (isOffer) {
          builder.buildOfferCET(msg)
        } else {
          builder.buildAcceptCET(msg)
        }
      }

      psbt <-
        PSBT
          .fromUnsignedTx(utx)
          .addWitnessUTXOToInput(fundingTx.outputs.head, index = 0)
          .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
          .sign(inputIndex = 0, fundingPrivKey)
    } yield {
      psbt
    }
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcome hash */
  def createRemoteCETSig(msg: Sha256DigestBE): Future[PartialSignature] = {
    for {
      psbt <- createPartiallySignedCET(msg, !isInitiator)
      signature <- findSigInPSBT(psbt, fundingPrivKey.publicKey)
    } yield {
      signature
    }
  }

  def signCET(
      msg: Sha256DigestBE,
      remoteSig: PartialSignature): Future[Transaction] = {
    for {
      unsignedPsbt <- createPartiallySignedCET(msg, isInitiator)
      psbt = unsignedPsbt.addSignature(remoteSig, inputIndex = 0)

      cetT = psbt.finalizePSBT.flatMap(_.extractTransactionAndValidate)
      cet <- Future.fromTry(cetT)
    } yield {
      cet
    }
  }

  /** Creates a PSBT of the refund transaction which contain's this
    * party's signature
    */
  def createPartiallySignedRefundTx(): Future[PSBT] = {
    for {
      fundingTx <- builder.buildFundingTx
      utx <- builder.buildRefundTx
      psbt <-
        PSBT
          .fromUnsignedTx(utx)
          .addWitnessUTXOToInput(fundingTx.outputs.head, index = 0)
          .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
          .sign(inputIndex = 0, fundingPrivKey)
    } yield {
      psbt
    }
  }

  /** Creates this party's signature of the refund transaction */
  def createRefundSig(): Future[PartialSignature] = {
    for {
      psbt <- createPartiallySignedRefundTx()
      signature <- findSigInPSBT(psbt, fundingPrivKey.publicKey)
    } yield {
      signature
    }
  }

  /** Constructs the signed refund transaction given remote's signature */
  def signRefundTx(remoteSig: PartialSignature): Future[Transaction] = {
    for {
      unsignedPSBT <- createPartiallySignedRefundTx()
      psbt = unsignedPSBT.addSignature(remoteSig, inputIndex = 0)

      refundTxT = psbt.finalizePSBT.flatMap(_.extractTransactionAndValidate)
      refundTx <- Future.fromTry(refundTxT)
    } yield {
      refundTx
    }
  }

  /** Creates all of this party's CETSignatures */
  def createCETSigs(): Future[CETSignatures] = {
    val cetSigFs = offer.contractInfo.keys.toVector.map { msg =>
      createRemoteCETSig(msg).map(msg -> _)
    }

    for {
      cetSigs <- Future.sequence(cetSigFs).map(_.toMap)
      refundSig <- createRefundSig()
    } yield CETSignatures(cetSigs, refundSig)
  }
}

object DLCTxSigner {

  def apply(
      offer: DLCOffer,
      accept: DLCAcceptWithoutSigs,
      isInitiator: Boolean,
      extPrivKey: ExtPrivateKey,
      nextAddressIndex: Int,
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]])(implicit
      ec: ExecutionContext): DLCTxSigner = {
    DLCTxSigner(DLCTxBuilder(offer, accept),
                isInitiator,
                extPrivKey,
                nextAddressIndex,
                fundingUtxos)
  }

  /** Computes the tweaked private key used to claim a
    * Contract Execution Transaction's to_local output
    * given an oracle signature
    */
  def tweakedPrivKey(
      fundingPrivKey: ECPrivateKey,
      cetToLocalPrivKey: ECPrivateKey,
      oracleSig: SchnorrDigitalSignature): ECPrivateKey = {
    val privKeyWithoutTweak = oracleSig.sig.add(fundingPrivKey.fieldElement)

    val tweakHash = CryptoUtil.sha256(cetToLocalPrivKey.publicKey.bytes).flip
    val tweak = FieldElement(tweakHash.bytes)

    privKeyWithoutTweak.add(tweak).toPrivateKey
  }
}
