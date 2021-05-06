package org.bitcoins.dlc.sign

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{
  TransactionSignatureCreator,
  TransactionSignatureSerializer
}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** Responsible for constructing all DLC signatures
  * and signed transactions
  */
case class DLCTxSigner(
    builder: DLCTxBuilder,
    isInitiator: Boolean,
    fundingKey: AdaptorSign,
    finalAddress: BitcoinAddress,
    fundingUtxos: Vector[ScriptSignatureParams[InputInfo]]) {

  private val offer = builder.offer
  private val accept = builder.accept

  private val remoteFundingPubKey = if (isInitiator) {
    accept.pubKeys.fundingKey
  } else {
    offer.pubKeys.fundingKey
  }

  if (isInitiator) {
    require(fundingKey.publicKey == offer.pubKeys.fundingKey &&
              finalAddress == offer.pubKeys.payoutAddress,
            "Given keys do not match public key and address in offer")
    val fundingUtxosAsInputs =
      fundingUtxos.zip(offer.fundingInputs).map { case (utxo, fund) =>
        DLCFundingInput.fromInputSigningInfo(utxo, fund.inputSerialId)
      }
    require(fundingUtxosAsInputs == offer.fundingInputs,
            "Funding ScriptSignatureParams did not match offer funding inputs")
  } else {
    require(
      fundingKey.publicKey == accept.pubKeys.fundingKey &&
        finalAddress == accept.pubKeys.payoutAddress,
      "Given keys do not match public key and address in accept"
    )
    val fundingUtxosAsInputs =
      fundingUtxos.zip(accept.fundingInputs).map { case (utxo, fund) =>
        DLCFundingInput.fromInputSigningInfo(utxo, fund.inputSerialId)
      }
    require(fundingUtxosAsInputs == accept.fundingInputs,
            "Funding ScriptSignatureParams did not match accept funding inputs")
  }

  /** Return's this party's payout for a given oracle signature */
  def getPayout(sigs: Vector[OracleSignatures]): CurrencyUnit = {
    val (offerPayout, acceptPayout) = builder.getPayouts(sigs)
    if (isInitiator) {
      offerPayout
    } else {
      acceptPayout
    }
  }

  /** Creates this party's FundingSignatures */
  def signFundingTx(): Try[FundingSignatures] = {
    val fundingInputs =
      if (isInitiator) builder.offerFundingInputs
      else builder.acceptFundingInputs

    val utxos =
      fundingUtxos
        .zip(fundingInputs)
        .map { case (utxo, fundingInput) =>
          SpendingInfoWithSerialId(utxo, fundingInput.inputSerialId)
        }
        .sortBy(_.serialId)

    DLCTxSigner.signFundingTx(builder.buildFundingTx, utxos)
  }

  /** Constructs the signed DLC funding transaction given remote FundingSignatures */
  def completeFundingTx(remoteSigs: FundingSignatures): Try[Transaction] = {
    signFundingTx().flatMap { localSigs =>
      DLCTxSigner.completeFundingTx(localSigs,
                                    remoteSigs,
                                    offer.fundingInputs,
                                    accept.fundingInputs,
                                    builder.buildFundingTx)
    }
  }

  private var _cetSigningInfo: Option[ECSignatureParams[P2WSHV0InputInfo]] =
    None

  private def cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo] = {
    _cetSigningInfo match {
      case Some(info) => info
      case None =>
        val signingInfo =
          DLCTxSigner.buildCETSigningInfo(builder.fundOutputIndex,
                                          builder.buildFundingTx,
                                          builder.fundingMultiSig,
                                          fundingKey)

        _cetSigningInfo = Some(signingInfo)

        signingInfo
    }
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcome */
  def signCET(outcome: OracleOutcome): ECAdaptorSignature = {
    signCETs(Vector(outcome)).head._2
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcomes */
  def buildAndSignCETs(outcomes: Vector[OracleOutcome]): Vector[
    (OracleOutcome, WitnessTransaction, ECAdaptorSignature)] = {
    val outcomesAndCETs = builder.buildCETsMap(outcomes)
    DLCTxSigner.buildAndSignCETs(outcomesAndCETs, cetSigningInfo, fundingKey)
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcomes */
  def signCETs(outcomes: Vector[OracleOutcome]): Vector[
    (OracleOutcome, ECAdaptorSignature)] = {
    buildAndSignCETs(outcomes).map { case (outcome, _, sig) =>
      outcome -> sig
    }
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcomes and their corresponding CETs */
  def signGivenCETs(outcomesAndCETs: Vector[OutcomeCETPair]): Vector[
    (OracleOutcome, ECAdaptorSignature)] = {
    DLCTxSigner.signCETs(outcomesAndCETs, cetSigningInfo, fundingKey)
  }

  def completeCET(
      outcome: OracleOutcome,
      remoteAdaptorSig: ECAdaptorSignature,
      oracleSigs: Vector[OracleSignatures]): WitnessTransaction = {
    DLCTxSigner.completeCET(
      outcome,
      cetSigningInfo,
      builder.fundingMultiSig,
      builder.buildFundingTx,
      builder.buildCET(outcome),
      remoteAdaptorSig,
      remoteFundingPubKey,
      oracleSigs
    )
  }

  /** Creates this party's signature of the refund transaction */
  lazy val signRefundTx: PartialSignature = {
    DLCTxSigner.signRefundTx(cetSigningInfo, builder.buildRefundTx)
  }

  /** Constructs the signed refund transaction given remote's signature */
  def completeRefundTx(remoteSig: PartialSignature): WitnessTransaction = {
    val localSig = signRefundTx

    DLCTxSigner.completeRefundTx(localSig,
                                 remoteSig,
                                 builder.fundingMultiSig,
                                 builder.buildFundingTx,
                                 builder.buildRefundTx)
  }

  /** Creates all of this party's CETSignatures */
  def createCETSigs(): CETSignatures = {
    val cetSigs = signCETs(builder.contractInfo.allOutcomes)
    val refundSig = signRefundTx

    CETSignatures(cetSigs, refundSig)
  }

  /** Computes the CET sigs asynchronously */
  def createCETSigsAsync()(implicit
      ec: ExecutionContext): Future[CETSignatures] = {
    val startOutcomes = System.currentTimeMillis()
    val outcomes = builder.contractInfo.allOutcomes
    val startSigning = System.currentTimeMillis()

    //divide and conquer
    val size = outcomes.length % Runtime.getRuntime.availableProcessors()

    //this gives us a iterator of size Runtime.getRuntime.availableProcess()
    val dividedOutcomes: Iterator[Vector[OracleOutcome]] = {
      outcomes.grouped(size)
    }

    //compute all the cets async
    val cetSigsAsyncNested: Iterator[
      Future[Vector[(OracleOutcome, ECAdaptorSignature)]]] = {
      dividedOutcomes.map { o =>
        Future {
          signCETs(o)
        }
      }
    }

    //aggregate everything
    val cetSigsF: Future[Vector[(OracleOutcome, ECAdaptorSignature)]] = {
      Future
        .sequence(cetSigsAsyncNested)
        .map(_.flatten.toVector)
    }

    for {
      cetSigs <- cetSigsF
      refundSig = signRefundTx
    } yield CETSignatures(cetSigs, refundSig)
  }

  /** Creates all of this party's CETSignatures */
  def createCETsAndCETSigs(): (CETSignatures, Vector[WitnessTransaction]) = {
    val cetsAndSigs = buildAndSignCETs(builder.contractInfo.allOutcomes)
    val (msgs, cets, sigs) = cetsAndSigs.unzip3
    val refundSig = signRefundTx

    (CETSignatures(msgs.zip(sigs), refundSig), cets)
  }

  /** Creates this party's CETSignatures given the outcomes and their unsigned CETs */
  def createCETSigs(outcomesAndCETs: Vector[OutcomeCETPair]): CETSignatures = {
    val cetSigs = signGivenCETs(outcomesAndCETs)
    val refundSig = signRefundTx

    CETSignatures(cetSigs, refundSig)
  }
}

object DLCTxSigner {

  def apply(
      builder: DLCTxBuilder,
      isInitiator: Boolean,
      fundingKey: AdaptorSign,
      payoutPrivKey: AdaptorSign,
      network: BitcoinNetwork,
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]]): DLCTxSigner = {
    val payoutAddr =
      Bech32Address(P2WPKHWitnessSPKV0(payoutPrivKey.publicKey), network)
    DLCTxSigner(builder, isInitiator, fundingKey, payoutAddr, fundingUtxos)
  }

  def buildCETSigningInfo(
      fundOutputIndex: Int,
      fundingTx: Transaction,
      fundingMultiSig: MultiSignatureScriptPubKey,
      fundingKey: Sign): ECSignatureParams[P2WSHV0InputInfo] = {
    val fundingOutPoint =
      TransactionOutPoint(fundingTx.txId, UInt32(fundOutputIndex))

    ECSignatureParams(
      P2WSHV0InputInfo(
        outPoint = fundingOutPoint,
        amount = fundingTx.outputs(fundOutputIndex).value,
        scriptWitness = P2WSHWitnessV0(fundingMultiSig),
        conditionalPath = ConditionalPath.NoCondition
      ),
      fundingTx,
      fundingKey,
      HashType.sigHashAll
    )
  }

  def signCET(
      outcome: OracleOutcome,
      cet: WitnessTransaction,
      cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo],
      fundingKey: AdaptorSign): ECAdaptorSignature = {
    signCETs(Vector(OutcomeCETPair(outcome, cet)),
             cetSigningInfo,
             fundingKey).head._2
  }

  def signCETs(
      outcomesAndCETs: Vector[OutcomeCETPair],
      cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo],
      fundingKey: AdaptorSign): Vector[(OracleOutcome, ECAdaptorSignature)] = {
    buildAndSignCETs(outcomesAndCETs, cetSigningInfo, fundingKey).map {
      case (outcome, _, sig) => outcome -> sig
    }
  }

  def buildAndSignCETs(
      outcomesAndCETs: Vector[OutcomeCETPair],
      cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo],
      fundingKey: AdaptorSign): Vector[
    (OracleOutcome, WitnessTransaction, ECAdaptorSignature)] = {
    outcomesAndCETs.map { case OutcomeCETPair(outcome, cet) =>
      val adaptorPoint = outcome.sigPoint
      val hashToSign =
        TransactionSignatureSerializer.hashForSignature(cet,
                                                        cetSigningInfo,
                                                        HashType.sigHashAll)

      val adaptorSig = fundingKey.adaptorSign(adaptorPoint, hashToSign.bytes)
      (outcome, cet, adaptorSig)
    }
  }

  // TODO: Without PSBTs
  def completeCET(
      outcome: OracleOutcome,
      cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo],
      fundingMultiSig: MultiSignatureScriptPubKey,
      fundingTx: Transaction,
      ucet: WitnessTransaction,
      remoteAdaptorSig: ECAdaptorSignature,
      remoteFundingPubKey: ECPublicKey,
      oracleSigs: Vector[OracleSignatures]): WitnessTransaction = {
    val signLowR: ByteVector => ECDigitalSignature =
      cetSigningInfo.signer.signLowR(_: ByteVector)
    val localSig = TransactionSignatureCreator.createSig(ucet,
                                                         cetSigningInfo,
                                                         signLowR,
                                                         HashType.sigHashAll)
    val oracleSigSum =
      OracleSignatures.computeAggregateSignature(outcome, oracleSigs)

    val remoteSig =
      oracleSigSum
        .completeAdaptorSignature(remoteAdaptorSig, HashType.sigHashAll.byte)

    val localParitalSig =
      PartialSignature(cetSigningInfo.signer.publicKey, localSig)
    val remotePartialSig = PartialSignature(remoteFundingPubKey, remoteSig)

    val psbt =
      PSBT
        .fromUnsignedTx(ucet)
        .addUTXOToInput(fundingTx, index = 0)
        .addScriptWitnessToInput(P2WSHWitnessV0(fundingMultiSig), index = 0)
        .addSignature(localParitalSig, inputIndex = 0)
        .addSignature(remotePartialSig, inputIndex = 0)

    val cetT = psbt.finalizePSBT
      .flatMap(_.extractTransactionAndValidate)
      .map(_.asInstanceOf[WitnessTransaction])

    cetT match {
      case Success(cet) => cet.asInstanceOf[WitnessTransaction]
      case Failure(err) => throw err
    }
  }

  def signRefundTx(
      refundSigningInfo: ECSignatureParams[P2WSHV0InputInfo],
      refundTx: WitnessTransaction
  ): PartialSignature = {
    val fundingPubKey = refundSigningInfo.signer.publicKey

    val signLowR: ByteVector => ECDigitalSignature =
      refundSigningInfo.signer.signLowR(_: ByteVector)
    val sig = TransactionSignatureCreator.createSig(refundTx,
                                                    refundSigningInfo,
                                                    signLowR,
                                                    HashType.sigHashAll)

    PartialSignature(fundingPubKey, sig)
  }

  // TODO: Without PSBTs
  def completeRefundTx(
      localSig: PartialSignature,
      remoteSig: PartialSignature,
      fundingMultiSig: MultiSignatureScriptPubKey,
      fundingTx: Transaction,
      uRefundTx: WitnessTransaction): WitnessTransaction = {
    val psbt = PSBT
      .fromUnsignedTx(uRefundTx)
      .addUTXOToInput(fundingTx, index = 0)
      .addScriptWitnessToInput(P2WSHWitnessV0(fundingMultiSig), index = 0)
      .addSignature(localSig, inputIndex = 0)
      .addSignature(remoteSig, inputIndex = 0)

    val refundTxT = psbt.finalizePSBT
      .flatMap(_.extractTransactionAndValidate)
      .map(_.asInstanceOf[WitnessTransaction])

    refundTxT match {
      case Success(refundTx) => refundTx
      case Failure(err)      => throw err
    }
  }

  def signFundingTx(
      fundingTx: Transaction,
      fundingUtxos: Vector[SpendingInfoWithSerialId]
  ): Try[FundingSignatures] = {
    val sigsT = fundingUtxos
      .foldLeft[Try[Vector[(TransactionOutPoint, ScriptWitnessV0)]]](
        Success(Vector.empty)) {
        case (sigsT, SpendingInfoWithSerialId(utxo, _)) =>
          sigsT.flatMap { sigs =>
            val sigComponent =
              BitcoinSigner.sign(utxo, fundingTx, isDummySignature = false)
            val witnessT =
              sigComponent.transaction match {
                case wtx: WitnessTransaction =>
                  val witness = wtx.witness(sigComponent.inputIndex.toInt)
                  if (witness == EmptyScriptWitness) {
                    Failure(
                      new RuntimeException(
                        s"Funding Inputs must be SegWit: $utxo"))
                  } else {
                    Success(witness)
                  }
                case _: NonWitnessTransaction =>
                  Failure(
                    new RuntimeException(
                      s"Funding Inputs must be SegWit: $utxo"))
              }

            witnessT.flatMap {
              case witness: ScriptWitnessV0 =>
                Success(sigs.:+((utxo.outPoint, witness)))
              case witness: ScriptWitness =>
                Failure(
                  new RuntimeException(
                    s"Unrecognized script witness: $witness"))
            }
          }
      }

    sigsT.map { sigs =>
      val sigsMap = sigs.toMap

      val fundingInputs: Vector[DLCFundingInput] =
        fundingUtxos.map { case SpendingInfoWithSerialId(utxo, serialId) =>
          DLCFundingInput.fromInputSigningInfo(utxo, serialId)
        }

      val sigsVec = fundingInputs.map { input =>
        input.outPoint -> sigsMap(input.outPoint)
      }

      FundingSignatures(sigsVec)
    }
  }

  def completeFundingTx(
      localSigs: FundingSignatures,
      remoteSigs: FundingSignatures,
      offerFundingInputs: Vector[DLCFundingInput],
      acceptFundingInputs: Vector[DLCFundingInput],
      fundingTx: Transaction): Try[Transaction] = {
    val fundingInputs =
      (offerFundingInputs ++ acceptFundingInputs).sortBy(_.inputSerialId)
    val allSigs = localSigs.merge(remoteSigs)

    val psbt = fundingInputs.zipWithIndex.foldLeft(
      PSBT.fromUnsignedTxWithP2SHScript(fundingTx)) {
      case (psbt, (fundingInput, index)) =>
        val witness = allSigs(fundingInput.outPoint)

        psbt
          .addUTXOToInput(fundingInput.prevTx, index)
          .addFinalizedScriptWitnessToInput(fundingInput.scriptSignature,
                                            witness,
                                            index)
    }

    val finalizedT = if (psbt.isFinalized) {
      Success(psbt)
    } else {
      psbt.finalizePSBT
    }

    finalizedT.flatMap(_.extractTransactionAndValidate)
  }
}
