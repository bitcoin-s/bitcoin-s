package org.bitcoins.dlc.sign

import org.bitcoins.commons.jsonmodels.dlc.{
  CETSignatures,
  DLCFundingInput,
  FundingSignatures
}
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.TransactionSignatureSerializer
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv.DLCOutcomeType
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

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

  private val fundingSPK: MultiSignatureScriptPubKey =
    builder.fundingTxBuilder.fundingMultiSig

  if (isInitiator) {
    require(fundingKey.publicKey == offer.pubKeys.fundingKey &&
              finalAddress == offer.pubKeys.payoutAddress,
            "Given keys do not match public key and address in offer")
    require(fundingUtxos.map(
              DLCFundingInput.fromInputSigningInfo(_)) == offer.fundingInputs,
            "Funding ScriptSignatureParams did not match offer funding inputs")
  } else {
    require(
      fundingKey.publicKey == accept.pubKeys.fundingKey &&
        finalAddress == accept.pubKeys.payoutAddress,
      "Given keys do not match public key and address in accept"
    )
    require(fundingUtxos.map(
              DLCFundingInput.fromInputSigningInfo(_)) == accept.fundingInputs,
            "Funding ScriptSignatureParams did not match accept funding inputs")
  }

  /** Return's this party's payout for a given oracle signature */
  def getPayout(sigs: Vector[SchnorrDigitalSignature]): CurrencyUnit = {
    val (offerPayout, acceptPayout) = builder.getPayouts(sigs)
    if (isInitiator) {
      offerPayout
    } else {
      acceptPayout
    }
  }

  /** Creates this party's FundingSignatures */
  def createFundingTxSigs(): Future[FundingSignatures] = {
    val sigFs =
      Vector.newBuilder[Future[(TransactionOutPoint, ScriptWitnessV0)]]

    for {
      fundingTx <- builder.buildFundingTx

      _ = {
        fundingUtxos.foreach { utxo =>
          val sigComponentF =
            BitcoinSigner.sign(utxo, fundingTx, isDummySignature = false)
          val witnessF = sigComponentF.flatMap { sigComponent =>
            sigComponent.transaction match {
              case wtx: WitnessTransaction =>
                val witness = wtx.witness(sigComponent.inputIndex.toInt)
                if (witness == EmptyScriptWitness) {
                  Future.failed(
                    new RuntimeException(
                      s"Funding Inputs must be SegWit: $utxo"))
                } else {
                  Future.successful(witness)
                }
              case _: NonWitnessTransaction =>
                Future.failed(
                  new RuntimeException(s"Funding Inputs must be SegWit: $utxo"))
            }
          }

          sigFs += witnessF.flatMap {
            case witness: ScriptWitnessV0 =>
              Future.successful((utxo.outPoint, witness))
            case witness: ScriptWitness =>
              Future.failed(
                new RuntimeException(s"Unrecognized script witness: $witness"))
          }
        }
      }

      sigs <- Future.sequence(sigFs.result())
    } yield {
      val sigsMap = sigs.toMap

      val fundingInputs = if (isInitiator) {
        offer.fundingInputs
      } else {
        accept.fundingInputs
      }

      val sigsVec = fundingInputs.map { input =>
        input.outPoint -> sigsMap(input.outPoint)
      }

      FundingSignatures(sigsVec)
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
      fundingInputs.zipWithIndex.foldLeft(
        PSBT.fromUnsignedTxWithP2SHScript(fundingTx)) {
        case (psbt, (fundingInput, index)) =>
          val witness = allSigs(fundingInput.outPoint)

          psbt
            .addUTXOToInput(fundingInput.prevTx, index)
            .addFinalizedScriptWitnessToInput(fundingInput.scriptSignature,
                                              witness,
                                              index)
      }
    }

    psbtF.flatMap { psbt =>
      val finalizedT = if (psbt.isFinalized) {
        Success(psbt)
      } else {
        psbt.finalizePSBT
      }

      val txT = finalizedT.flatMap(_.extractTransactionAndValidate)
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
  def createRemoteCETSig(msg: DLCOutcomeType): Future[ECAdaptorSignature] = {
    val adaptorPoint = builder.oracleAndContractInfo.sigPointForOutcome(msg)
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
      msg: DLCOutcomeType,
      remoteAdaptorSig: ECAdaptorSignature,
      oracleSigs: Vector[SchnorrDigitalSignature]): Future[Transaction] = {
    val oracleSigSum = oracleSigs.map(_.sig).reduce(_.add(_)).toPrivateKey
    val remoteSig =
      oracleSigSum
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
    val cetSigFs = builder.oracleAndContractInfo.allOutcomes.map { msg =>
      // Need to wrap in another future so they are all started at once
      // and do not block each other
      Future(createRemoteCETSig(msg).map(msg -> _)).flatten
    }

    for {
      cetSigs <- Future.sequence(cetSigFs)
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
