package org.bitcoins.dlc.sign

import org.bitcoins.commons.jsonmodels.dlc.{CETSignatures, FundingSignatures}
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.TransactionSignatureSerializer
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WSHWitnessV0
}
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  Transaction,
  TransactionOutPoint,
  TxUtil
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder

import scala.concurrent.{ExecutionContext, Future}

/** Responsible for constructing all DLC signatures
  * and signed transactions
  */
case class DLCTxSigner(
    builder: DLCTxBuilder,
    isInitiator: Boolean,
    fundingKey: ECPrivateKey,
    finalAddress: BitcoinAddress,
    fundingUtxos: Vector[ScriptSignatureParams[InputInfo]])(implicit
    ec: ExecutionContext) {

  private val offer = builder.offer
  private val accept = builder.accept

  private val remoteFundingPubKey = if (isInitiator) {
    accept.pubKeys.fundingKey
  } else {
    offer.pubKeys.fundingKey
  }

  private val fundingSPK = MultiSignatureScriptPubKey(
    2,
    Vector(offer.pubKeys.fundingKey, accept.pubKeys.fundingKey)
  )

  if (isInitiator) {
    require(fundingKey.publicKey == offer.pubKeys.fundingKey &&
              finalAddress == offer.pubKeys.payoutAddress,
            "Given keys do not match public key and address in offer")
    require(fundingUtxos.map(_.outputReference) == offer.fundingInputs,
            "Funding ScriptSignatureParams did not match offer funding inputs")
  } else {
    require(
      fundingKey.publicKey == accept.pubKeys.fundingKey &&
        finalAddress == accept.pubKeys.payoutAddress,
      "Given keys do not match public key and address in accept"
    )
    require(fundingUtxos.map(_.outputReference) == accept.fundingInputs,
            "Funding ScriptSignatureParams did not match accept funding inputs")
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
            .addSignatures(sigs, index)
            .addWitnessUTXOToInput(output, index)
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

  /** Signs remote's Contract Execution Transaction (CET) for a given outcome hash */
  def createRemoteCETSig(msg: Sha256DigestBE): Future[ECAdaptorSignature] = {
    val adaptorPoint = builder.sigPubKeys(msg)
    val hashType = HashType.sigHashAll
    for {
      fundingTx <- builder.buildFundingTx
      fundingOutPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)
      utx <- builder.buildCET(msg)
      signingInfo = ECSignatureParams(
        P2WSHV0InputInfo(outPoint = fundingOutPoint,
                         amount = fundingTx.outputs.head.value,
                         scriptWitness = P2WSHWitnessV0(fundingSPK),
                         conditionalPath = ConditionalPath.NoCondition),
        fundingTx,
        fundingKey,
        hashType
      )
      utxWithData = TxUtil.addWitnessData(utx, signingInfo)
      hashToSign = TransactionSignatureSerializer.hashForSignature(utxWithData,
                                                                   signingInfo,
                                                                   hashType)
    } yield {
      fundingKey.adaptorSign(adaptorPoint, hashToSign.bytes)
    }
  }

  def signCET(
      msg: Sha256DigestBE,
      remoteAdaptorSig: ECAdaptorSignature,
      oracleSig: SchnorrDigitalSignature): Future[Transaction] = {
    val remoteSig =
      oracleSig.sig.toPrivateKey
        .completeAdaptorSignature(remoteAdaptorSig, HashType.sigHashAll.byte)

    val remotePartialSig = PartialSignature(remoteFundingPubKey, remoteSig)
    for {
      fundingTx <- builder.buildFundingTx
      utx <- builder.buildCET(msg)

      psbt <-
        PSBT
          .fromUnsignedTx(utx)
          .addUTXOToInput(fundingTx, index = 0)
          .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
          .addSignature(remotePartialSig, inputIndex = 0)
          .sign(inputIndex = 0, fundingKey)

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
          .addUTXOToInput(fundingTx, index = 0)
          .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
          .sign(inputIndex = 0, fundingKey)
    } yield {
      psbt
    }
  }

  /** Creates this party's signature of the refund transaction */
  def createRefundSig(): Future[PartialSignature] = {
    for {
      psbt <- createPartiallySignedRefundTx()
      signature <- findSigInPSBT(psbt, fundingKey.publicKey)
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
      builder: DLCTxBuilder,
      isInitiator: Boolean,
      fundingKey: ECPrivateKey,
      payoutPrivKey: ECPrivateKey,
      network: BitcoinNetwork,
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]])(implicit
      ec: ExecutionContext): DLCTxSigner = {
    val payoutAddr =
      Bech32Address(P2WPKHWitnessSPKV0(payoutPrivKey.publicKey), network)
    DLCTxSigner(builder, isInitiator, fundingKey, payoutAddr, fundingUtxos)
  }
}
