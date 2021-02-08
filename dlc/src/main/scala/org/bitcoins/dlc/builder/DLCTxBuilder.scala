package org.bitcoins.dlc.builder

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.DLCMessage._
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.wallet.builder.{
  DualFundingTxFinalizer,
  RawTxBuilderWithFinalizer
}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  InputInfo,
  P2WSHV0InputInfo
}
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

case class DLCTxBuilder(offer: DLCOffer, accept: DLCAcceptWithoutSigs)(implicit
    ec: ExecutionContext) {

  val DLCOffer(_,
               DLCPublicKeys(offerFundingKey: ECPublicKey,
                             offerFinalAddress: BitcoinAddress),
               offerTotalCollateral: Satoshis,
               offerFundingInputs: Vector[DLCFundingInput],
               offerChangeAddress: BitcoinAddress,
               feeRate: SatoshisPerVirtualByte,
               DLCTimeouts(contractMaturity: BlockTimeStamp,
                           contractTimeout: BlockTimeStamp)) = offer

  val network: BitcoinNetwork = offerFinalAddress.networkParameters match {
    case network: BitcoinNetwork => network
  }

  val DLCAcceptWithoutSigs(acceptTotalCollateral: Satoshis,
                           DLCPublicKeys(acceptFundingKey: ECPublicKey,
                                         acceptFinalAddress: BitcoinAddress),
                           acceptFundingInputs: Vector[DLCFundingInput],
                           acceptChangeAddress: BitcoinAddress,
                           acceptNegotiationFields: DLCAccept.NegotiationFields,
                           tempContractId: Sha256Digest) = accept

  val totalInput: CurrencyUnit = offerTotalCollateral + acceptTotalCollateral

  // builder.offer.oracleAndContractInfo should not be used,
  // builder.oracleAndContractInfo should be used instead in case a party
  // is over-collateralized in which case payouts will be incorrect here.
  private val contractInfoBeforeAccept: ContractInfo =
    offer.contractInfo

  val contractInfo: ContractInfo =
    contractInfoBeforeAccept.updateOnAccept(totalInput.satoshis,
                                            acceptNegotiationFields)

  val offerTotalFunding: CurrencyUnit =
    offerFundingInputs.map(_.output.value).sum

  val acceptTotalFunding: CurrencyUnit =
    acceptFundingInputs.map(_.output.value).sum

  require(offer.tempContractId == tempContractId,
          "Offer and accept (without sigs) must refer to same event")
  require(acceptFinalAddress.networkParameters == network,
          "Offer and accept (without sigs) must be on the same network")
  require(offerChangeAddress.networkParameters == network,
          "Offer change address must have same network as final address")
  require(acceptChangeAddress.networkParameters == network,
          "Accept change address must have same network as final address")
  require(totalInput >= contractInfo.max,
          "Total collateral must add up to max winnings")
  require(
    offerTotalFunding >= offerTotalCollateral,
    "Offer funding inputs must add up to at least offer's total collateral")
  require(
    acceptTotalFunding >= acceptTotalCollateral,
    "Accept funding inputs must add up to at least accept's total collateral")

  /** Returns the payouts for the signature as (toOffer, toAccept) */
  def getPayouts(
      oracleSigs: Vector[OracleSignatures]): (CurrencyUnit, CurrencyUnit) = {
    contractInfo.getPayouts(oracleSigs)
  }

  val fundingKeys: Vector[ECPublicKey] =
    Vector(offerFundingKey, acceptFundingKey).sortBy(_.hex)

  /** The 2-of-2 MultiSignatureScriptPubKey to be wrapped in P2WSH and used as the funding output,
    * and the funding output's P2WSH(MultiSig) ScriptPubKey
    */
  val (fundingMultiSig: MultiSignatureScriptPubKey,
       fundingSPK: P2WSHWitnessSPKV0) =
    DLCTxBuilder.buildFundingSPKs(fundingKeys)

  lazy val fundingTxFinalizer: DualFundingTxFinalizer = {
    DLCTxBuilder.buildFundingTxFinalizer(
      offerFundingInputs = offerFundingInputs,
      acceptFundingInputs = acceptFundingInputs,
      offerChangeSPK = offerChangeAddress.scriptPubKey,
      acceptChangeSPK = acceptChangeAddress.scriptPubKey,
      offerPayoutSPK = offerFinalAddress.scriptPubKey,
      acceptPayoutSPK = acceptFinalAddress.scriptPubKey,
      feeRate = feeRate,
      fundingSPK = fundingSPK
    )
  }

  /** Constructs the unsigned funding transaction */
  lazy val buildFundingTx: Future[Transaction] = {
    DLCTxBuilder.buildFundingTransaction(
      offerInput = offerTotalCollateral,
      acceptInput = acceptTotalCollateral,
      offerFundingInputs = offerFundingInputs,
      acceptFundingInputs = acceptFundingInputs,
      offerChangeSPK = offerChangeAddress.scriptPubKey,
      acceptChangeSPK = acceptChangeAddress.scriptPubKey,
      fundingSPK = fundingSPK,
      fundingTxFinalizer = fundingTxFinalizer
    )
  }

  lazy val calcContractId: Future[ByteVector] = {
    buildFundingTx.map(fundingTx =>
      DLCMessage.computeContractId(fundingTx, accept.tempContractId))
  }

  /** Constructs the unsigned Contract Execution Transaction (CET)
    * for a given outcome hash
    */
  def buildCET(msg: OracleOutcome): Future[WitnessTransaction] = {
    buildCETs(Vector(msg)).map(_.head)
  }

  def buildCETsMap(msgs: Vector[OracleOutcome]): Future[
    Vector[(OracleOutcome, WitnessTransaction)]] = {
    buildFundingTx.map { fundingTx =>
      DLCTxBuilder
        .buildCETs(msgs,
                   contractInfo,
                   offerFundingKey,
                   offerFinalAddress.scriptPubKey,
                   acceptFundingKey,
                   acceptFinalAddress.scriptPubKey,
                   offer.timeouts,
                   fundingTx)
    }
  }

  def buildCETs(
      msgs: Vector[OracleOutcome]): Future[Vector[WitnessTransaction]] = {
    buildCETsMap(msgs).map(_.map(_._2))
  }

  /** Constructs the unsigned refund transaction */
  lazy val buildRefundTx: Future[WitnessTransaction] = {
    buildFundingTx.map { fundingTx =>
      DLCTxBuilder.buildRefundTx(
        offerTotalCollateral,
        offerFundingKey,
        offerFinalAddress.scriptPubKey,
        acceptTotalCollateral,
        acceptFundingKey,
        acceptFinalAddress.scriptPubKey,
        fundingTx,
        offer.timeouts
      )
    }
  }
}

object DLCTxBuilder {

  def buildFundingSPKs(fundingPubKeys: Vector[ECPublicKey]): (
      MultiSignatureScriptPubKey,
      P2WSHWitnessSPKV0) = {
    require(fundingPubKeys.length == 2,
            s"There must be exactly 2 funding keys, got $fundingPubKeys")
    val multiSigSPK =
      MultiSignatureScriptPubKey(2, fundingPubKeys.sortBy(_.hex))
    val p2wshSPK = P2WSHWitnessSPKV0(multiSigSPK)

    (multiSigSPK, p2wshSPK)
  }

  def buildFundingTxFinalizer(
      offerFundingInputs: Vector[DLCFundingInput],
      acceptFundingInputs: Vector[DLCFundingInput],
      offerChangeSPK: ScriptPubKey,
      acceptChangeSPK: ScriptPubKey,
      offerPayoutSPK: ScriptPubKey,
      acceptPayoutSPK: ScriptPubKey,
      feeRate: SatoshisPerVirtualByte,
      fundingSPK: P2WSHWitnessSPKV0): DualFundingTxFinalizer = {
    DualFundingTxFinalizer(
      offerInputs = offerFundingInputs.map(_.toDualFundingInput),
      offerPayoutSPK = offerPayoutSPK,
      offerChangeSPK = offerChangeSPK,
      acceptInputs = acceptFundingInputs.map(_.toDualFundingInput),
      acceptPayoutSPK = acceptPayoutSPK,
      acceptChangeSPK = acceptChangeSPK,
      feeRate = feeRate,
      fundingSPK = fundingSPK
    )
  }

  // TODO: De-futurify
  def buildFundingTransaction(
      offerInput: CurrencyUnit,
      acceptInput: CurrencyUnit,
      offerFundingInputs: Vector[DLCFundingInput],
      acceptFundingInputs: Vector[DLCFundingInput],
      offerChangeSPK: ScriptPubKey,
      acceptChangeSPK: ScriptPubKey,
      fundingSPK: P2WSHWitnessSPKV0,
      fundingTxFinalizer: DualFundingTxFinalizer)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    // The total collateral of both parties combined
    val totalInput: CurrencyUnit = offerInput + acceptInput

    // The sum of all funding input amounts from the initiator
    val offerTotalFunding: CurrencyUnit =
      offerFundingInputs.map(_.output.value).sum

    // The sum of all funding input amounts from the non-initiator
    val acceptTotalFunding: CurrencyUnit =
      acceptFundingInputs.map(_.output.value).sum

    require(
      offerTotalFunding >= offerInput,
      "Offer funding inputs must add up to at least offer's total collateral")
    require(
      acceptTotalFunding >= acceptInput,
      "Accept funding inputs must add up to at least accept's total collateral")

    val builder = RawTxBuilderWithFinalizer(fundingTxFinalizer)

    builder += TransactionOutput(totalInput, fundingSPK)
    builder += TransactionOutput(offerTotalFunding - offerInput, offerChangeSPK)
    builder += TransactionOutput(acceptTotalFunding - acceptInput,
                                 acceptChangeSPK)

    offerFundingInputs.foreach { ref =>
      val scriptSig = ref.redeemScriptOpt match {
        case Some(redeemScript) => P2SHScriptSignature(redeemScript)
        case None               => EmptyScriptSignature
      }

      builder += TransactionInput(ref.outPoint,
                                  scriptSig,
                                  TransactionConstants.sequence)
    }

    acceptFundingInputs.foreach { ref =>
      val scriptSig = ref.redeemScriptOpt match {
        case Some(redeemScript) => P2SHScriptSignature(redeemScript)
        case None               => EmptyScriptSignature
      }

      builder += TransactionInput(ref.outPoint,
                                  scriptSig,
                                  TransactionConstants.sequence)
    }

    builder.buildTx()
  }

  def buildCET(
      outcome: OracleOutcome,
      contractInfo: ContractInfo,
      offerFundingKey: ECPublicKey,
      offerFinalSPK: ScriptPubKey,
      acceptFundingKey: ECPublicKey,
      acceptFinalSPK: ScriptPubKey,
      timeouts: DLCTimeouts,
      fundingOutputRef: OutputReference): WitnessTransaction = {
    val Vector((_, cet)) = buildCETs(Vector(outcome),
                                     contractInfo,
                                     offerFundingKey,
                                     offerFinalSPK,
                                     acceptFundingKey,
                                     acceptFinalSPK,
                                     timeouts,
                                     fundingOutputRef)

    cet
  }

  def buildCETs(
      outcomes: Vector[OracleOutcome],
      contractInfo: ContractInfo,
      offerFundingKey: ECPublicKey,
      offerFinalSPK: ScriptPubKey,
      acceptFundingKey: ECPublicKey,
      acceptFinalSPK: ScriptPubKey,
      timeouts: DLCTimeouts,
      fundingOutputRef: OutputReference): Vector[
    (OracleOutcome, WitnessTransaction)] = {
    val builder =
      DLCCETBuilder(contractInfo,
                    offerFundingKey,
                    offerFinalSPK,
                    acceptFundingKey,
                    acceptFinalSPK,
                    timeouts,
                    fundingOutputRef)

    outcomes.map { outcome =>
      (outcome, builder.buildCET(outcome))
    }
  }

  def buildCETs(
      outcomes: Vector[OracleOutcome],
      contractInfo: ContractInfo,
      offerFundingKey: ECPublicKey,
      offerFinalSPK: ScriptPubKey,
      acceptFundingKey: ECPublicKey,
      acceptFinalSPK: ScriptPubKey,
      timeouts: DLCTimeouts,
      fundingTx: Transaction): Vector[(OracleOutcome, WitnessTransaction)] = {
    val fundingOutPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)
    val fundingOutputRef =
      OutputReference(fundingOutPoint, fundingTx.outputs.head)

    buildCETs(outcomes,
              contractInfo,
              offerFundingKey,
              offerFinalSPK,
              acceptFundingKey,
              acceptFinalSPK,
              timeouts,
              fundingOutputRef)
  }

  // TODO: clean up
  def buildRefundTx(
      offerInput: CurrencyUnit,
      offerFundingKey: ECPublicKey,
      offerFinalSPK: ScriptPubKey,
      acceptInput: CurrencyUnit,
      acceptFundingKey: ECPublicKey,
      acceptFinalSPK: ScriptPubKey,
      fundingOutputRef: OutputReference,
      timeouts: DLCTimeouts): WitnessTransaction = {
    val OutputReference(fundingOutPoint, fundingOutput) = fundingOutputRef
    val fundingKeys = Vector(offerFundingKey, acceptFundingKey).sortBy(_.hex)
    val fundingInfo = P2WSHV0InputInfo(
      outPoint = fundingOutPoint,
      amount = fundingOutput.value,
      scriptWitness =
        P2WSHWitnessV0(MultiSignatureScriptPubKey(2, fundingKeys)),
      conditionalPath = ConditionalPath.NoCondition
    )

    WitnessTransaction(
      TransactionConstants.validLockVersion,
      Vector(
        TransactionInput(fundingOutPoint,
                         EmptyScriptSignature,
                         TransactionConstants.disableRBFSequence)),
      Vector(TransactionOutput(offerInput, offerFinalSPK),
             TransactionOutput(acceptInput, acceptFinalSPK)),
      timeouts.contractTimeout.toUInt32,
      TransactionWitness.fromWitOpt(
        Vector(InputInfo.getScriptWitness(fundingInfo)))
    )
  }

  def buildRefundTx(
      offerInput: CurrencyUnit,
      offerFundingKey: ECPublicKey,
      offerFinalSPK: ScriptPubKey,
      acceptInput: CurrencyUnit,
      acceptFundingKey: ECPublicKey,
      acceptFinalSPK: ScriptPubKey,
      fundingTx: Transaction,
      timeouts: DLCTimeouts): WitnessTransaction = {
    val fundingOutPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)
    val fundingOutputRef =
      OutputReference(fundingOutPoint, fundingTx.outputs.head)

    buildRefundTx(offerInput,
                  offerFundingKey,
                  offerFinalSPK,
                  acceptInput,
                  acceptFundingKey,
                  acceptFinalSPK,
                  fundingOutputRef,
                  timeouts)
  }
}
