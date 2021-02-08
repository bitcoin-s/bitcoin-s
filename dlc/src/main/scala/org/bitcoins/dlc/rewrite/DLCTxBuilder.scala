package org.bitcoins.dlc.rewrite

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.dlc.{
  ContractInfo,
  DLCFundingInput,
  DLCTimeouts,
  OracleOutcome
}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
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
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.dlc.builder.DLCCETBuilder

import scala.concurrent.{ExecutionContext, Future}

object DLCTxBuilder {

  // TODO: Split up, de-futurify
  def buildFundingTransaction(
      offerFundingKey: ECPublicKey,
      acceptFundingKey: ECPublicKey,
      feeRate: SatoshisPerVirtualByte,
      offerInput: CurrencyUnit,
      acceptInput: CurrencyUnit,
      offerFundingInputs: Vector[DLCFundingInput],
      acceptFundingInputs: Vector[DLCFundingInput],
      offerChangeSPK: ScriptPubKey,
      acceptChangeSPK: ScriptPubKey,
      offerPayoutSPK: ScriptPubKey,
      acceptPayoutSPK: ScriptPubKey)(implicit
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

    val fundingKeys =
      Vector(offerFundingKey, acceptFundingKey).sortBy(_.hex)

    // The 2-of-2 MultiSignatureScriptPubKey to be wrapped in P2WSH and used as the funding output
    val fundingMultiSig: MultiSignatureScriptPubKey =
      MultiSignatureScriptPubKey(2, fundingKeys)

    // The funding output's P2WSH(MultiSig) ScriptPubKey
    val fundingSPK: P2WSHWitnessSPKV0 = P2WSHWitnessSPKV0(fundingMultiSig)

    val fundingTxFinalizer: DualFundingTxFinalizer = DualFundingTxFinalizer(
      offerInputs = offerFundingInputs.map(_.toDualFundingInput),
      offerPayoutSPK = offerPayoutSPK,
      offerChangeSPK = offerChangeSPK,
      acceptInputs = acceptFundingInputs.map(_.toDualFundingInput),
      acceptPayoutSPK = acceptPayoutSPK,
      acceptChangeSPK = acceptChangeSPK,
      feeRate = feeRate,
      fundingSPK = fundingSPK
    )
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
    val builder =
      DLCCETBuilder(contractInfo,
                    offerFundingKey,
                    offerFinalSPK,
                    acceptFundingKey,
                    acceptFinalSPK,
                    timeouts,
                    fundingOutputRef)

    builder.buildCET(outcome)
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
}
