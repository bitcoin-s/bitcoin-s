package org.bitcoins.dlc.builder

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{
  EmptyScriptSignature,
  MultiSignatureScriptPubKey,
  P2WSHWitnessSPKV0,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.builder.{
  P2WPKHDualFundingTxFinalizer,
  RawTxBuilderWithFinalizer
}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.crypto.ECPublicKey

import scala.concurrent.{ExecutionContext, Future}

case class DLCFundingTxBuilder(
    offerFundingKey: ECPublicKey,
    acceptFundingKey: ECPublicKey,
    feeRate: FeeUnit,
    offerInput: CurrencyUnit,
    acceptInput: CurrencyUnit,
    offerFundingInputs: Vector[OutputReference],
    acceptFundingInputs: Vector[OutputReference],
    offerChangeSPK: ScriptPubKey,
    acceptChangeSPK: ScriptPubKey) {
  val totalInput: CurrencyUnit = offerInput + acceptInput

  val offerTotalFunding: CurrencyUnit =
    offerFundingInputs.map(_.output.value).sum

  val acceptTotalFunding: CurrencyUnit =
    acceptFundingInputs.map(_.output.value).sum

  require(
    offerTotalFunding >= offerInput,
    "Offer funding inputs must add up to at least offer's total collateral")
  require(
    acceptTotalFunding >= acceptInput,
    "Accept funding inputs must add up to at least accept's total collateral")

  val fundingMultiSig: MultiSignatureScriptPubKey =
    MultiSignatureScriptPubKey(2, Vector(offerFundingKey, acceptFundingKey))

  val fundingSPK: P2WSHWitnessSPKV0 = P2WSHWitnessSPKV0(fundingMultiSig)

  private val spendingFee = DLCTxBuilder.approxCETVBytes + DLCTxBuilder.approxToLocalClosingVBytes

  val fundingTxFinalizer: P2WPKHDualFundingTxFinalizer =
    P2WPKHDualFundingTxFinalizer(spendingFee, feeRate, fundingSPK)

  def buildFundingTx()(implicit ec: ExecutionContext): Future[Transaction] = {
    val builder = RawTxBuilderWithFinalizer(fundingTxFinalizer)

    builder += TransactionOutput(totalInput, fundingSPK)
    builder += TransactionOutput(offerTotalFunding - offerInput, offerChangeSPK)
    builder += TransactionOutput(acceptTotalFunding - acceptInput,
                                 acceptChangeSPK)

    offerFundingInputs.foreach { ref =>
      builder += TransactionInput(ref.outPoint,
                                  EmptyScriptSignature,
                                  TransactionConstants.sequence)
    }

    acceptFundingInputs.foreach { ref =>
      builder += TransactionInput(ref.outPoint,
                                  EmptyScriptSignature,
                                  TransactionConstants.sequence)
    }

    builder.buildTx()
  }
}
