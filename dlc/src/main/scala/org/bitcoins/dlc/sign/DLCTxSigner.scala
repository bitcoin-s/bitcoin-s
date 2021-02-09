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
  def getPayout(sigs: Vector[OracleSignatures]): CurrencyUnit = {
    val (offerPayout, acceptPayout) = builder.getPayouts(sigs)
    if (isInitiator) {
      offerPayout
    } else {
      acceptPayout
    }
  }

  /** Creates this party's FundingSignatures */
  def signFundingTx(): Future[FundingSignatures] = {
    DLCTxSigner.signFundingTx(builder.buildFundingTx, fundingUtxos)
  }

  /** Constructs the signed DLC funding transaction given remote FundingSignatures */
  def completeFundingTx(remoteSigs: FundingSignatures): Future[Transaction] = {
    for {
      localSigs <- signFundingTx()
      signedTxT = DLCTxSigner.completeFundingTx(localSigs,
                                                remoteSigs,
                                                offer.fundingInputs,
                                                accept.fundingInputs,
                                                builder.buildFundingTx)
      signedTx <- Future.fromTry(signedTxT)
    } yield signedTx
  }

  private var _cetSigningInfo: Option[ECSignatureParams[P2WSHV0InputInfo]] =
    None

  private def cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo] = {
    _cetSigningInfo match {
      case Some(info) => info
      case None =>
        val signingInfo =
          DLCTxSigner.buildCETSigningInfo(builder.buildFundingTx,
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
  def signGivenCETs(
      outcomesAndCETs: Vector[(OracleOutcome, WitnessTransaction)]): Vector[
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

  /** Creates all of this party's CETSignatures */
  def createCETsAndCETSigs(): (CETSignatures, Vector[WitnessTransaction]) = {
    val cetsAndSigs = buildAndSignCETs(builder.contractInfo.allOutcomes)
    val (msgs, cets, sigs) = cetsAndSigs.unzip3
    val refundSig = signRefundTx

    (CETSignatures(msgs.zip(sigs), refundSig), cets)
  }

  /** Creates this party's CETSignatures given the outcomes and their unsigned CETs */
  def createCETSigs(
      outcomesAndCETs: Vector[
        (OracleOutcome, WitnessTransaction)]): CETSignatures = {
    val cetSigs = signGivenCETs(outcomesAndCETs)
    val refundSig = signRefundTx

    CETSignatures(cetSigs, refundSig)
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

  def buildCETSigningInfo(
      fundingTx: Transaction,
      fundingMultiSig: MultiSignatureScriptPubKey,
      fundingKey: ECPrivateKey): ECSignatureParams[P2WSHV0InputInfo] = {
    val fundingOutPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)

    ECSignatureParams(
      P2WSHV0InputInfo(
        outPoint = fundingOutPoint,
        amount = fundingTx.outputs.head.value,
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
      fundingKey: ECPrivateKey): ECAdaptorSignature = {
    signCETs(Vector((outcome, cet)), cetSigningInfo, fundingKey).head._2
  }

  def signCETs(
      outcomesAndCETs: Vector[(OracleOutcome, WitnessTransaction)],
      cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo],
      fundingKey: ECPrivateKey): Vector[(OracleOutcome, ECAdaptorSignature)] = {
    buildAndSignCETs(outcomesAndCETs, cetSigningInfo, fundingKey).map {
      case (outcome, _, sig) => outcome -> sig
    }
  }

  def buildAndSignCETs(
      outcomesAndCETs: Vector[(OracleOutcome, WitnessTransaction)],
      cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo],
      fundingKey: ECPrivateKey): Vector[
    (OracleOutcome, WitnessTransaction, ECAdaptorSignature)] = {
    outcomesAndCETs.map { case (outcome, cet) =>
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
      cetSigningInfo.signer.signLowR(_: ByteVector)(ExecutionContext.global)
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
      refundSigningInfo.signer.signLowR(_: ByteVector)(ExecutionContext.global)
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
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]]
  )(implicit ec: ExecutionContext): Future[FundingSignatures] = {
    val sigFs =
      Vector.newBuilder[Future[(TransactionOutPoint, ScriptWitnessV0)]]

    val fundingInputs: Vector[DLCFundingInput] =
      fundingUtxos.map(DLCFundingInput.fromInputSigningInfo(_))

    fundingUtxos.foreach { utxo =>
      val sigComponentF =
        BitcoinSigner.sign(utxo, fundingTx, isDummySignature = false)
      val witnessF = sigComponentF.flatMap { sigComponent =>
        sigComponent.transaction match {
          case wtx: WitnessTransaction =>
            val witness = wtx.witness(sigComponent.inputIndex.toInt)
            if (witness == EmptyScriptWitness) {
              Future.failed(
                new RuntimeException(s"Funding Inputs must be SegWit: $utxo"))
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

    val sigsF = Future.sequence(sigFs.result())

    sigsF.map { sigs =>
      val sigsMap = sigs.toMap

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
    val fundingInputs = offerFundingInputs ++ acceptFundingInputs
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
