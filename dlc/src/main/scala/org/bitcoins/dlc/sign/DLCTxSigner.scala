package org.bitcoins.dlc.sign

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.DLCMutualCloseSig
import org.bitcoins.commons.jsonmodels.dlc.{
  CETSignatures,
  DLCPublicKeys,
  FundingSignatures
}
import org.bitcoins.core.crypto.ExtPrivateKey
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
  ECPrivateKey,
  ECPublicKey,
  SchnorrDigitalSignature,
  Sha256DigestBE
}
import org.bitcoins.dlc.builder.DLCTxBuilder

import scala.concurrent.{ExecutionContext, Future}

case class DLCTxSigner(
    builder: DLCTxBuilder,
    isInitiator: Boolean,
    extPrivKey: ExtPrivateKey,
    nextAddressIndex: Int,
    fundingUtxos: Vector[ScriptSignatureParams[InputInfo]])(
    implicit ec: ExecutionContext) {
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
    require(fundingUtxos.map(_.outputReference) == accept.fundingInputs,
            "Funding ScriptSignatureParams did not match accept funding inputs")
  }

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
            sigFs.addOne(sigF.map((utxo.outPoint, _)))
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
      psbt = PSBT
        .fromUnsignedTx(utx)
        .addWitnessUTXOToInput(fundingTx.outputs.head, index = 0)
        .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
      signedPSBT <- psbt.sign(inputIndex = 0, fundingPrivKey)
    } yield {
      signedPSBT
    }
  }

  def createMutualCloseTxSig(
      oracleSig: SchnorrDigitalSignature): Future[DLCMutualCloseSig] = {
    for {
      psbt <- createPartiallySignedMutualCloseTx(oracleSig)
      sig <- findSigInPSBT(psbt, fundingPrivKey.publicKey)
    } yield {
      DLCMutualCloseSig(accept.eventId, oracleSig, sig)
    }
  }

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

      psbt <- PSBT
        .fromUnsignedTx(utx)
        .addWitnessUTXOToInput(fundingTx.outputs.head, index = 0)
        .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
        .sign(inputIndex = 0, fundingPrivKey)
    } yield {
      psbt
    }
  }

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

  def createPartiallySignedRefundTx(): Future[PSBT] = {
    for {
      fundingTx <- builder.buildFundingTx
      utx <- builder.buildRefundTx
      psbt <- PSBT
        .fromUnsignedTx(utx)
        .addWitnessUTXOToInput(fundingTx.outputs.head, index = 0)
        .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
        .sign(inputIndex = 0, fundingPrivKey)
    } yield {
      psbt
    }
  }

  def createRefundSig(): Future[PartialSignature] = {
    for {
      psbt <- createPartiallySignedRefundTx()
      signature <- findSigInPSBT(psbt, fundingPrivKey.publicKey)
    } yield {
      signature
    }
  }

  def signRefundTx(remoteSig: PartialSignature): Future[Transaction] = {
    for {
      unsignedPSBT <- createPartiallySignedRefundTx()
      psbt = unsignedPSBT.addSignature(remoteSig, inputIndex = 0)

      refundTxT = psbt.finalizePSBT.flatMap(_.extractTransactionAndValidate)
      refuntTx <- Future.fromTry(refundTxT)
    } yield {
      refuntTx
    }
  }

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
