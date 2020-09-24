package org.bitcoins.dlc.builder

import org.bitcoins.commons.jsonmodels.dlc.DLCFundingInput
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{
  EmptyScriptSignature,
  MultiSignatureScriptPubKey,
  P2WSHWitnessSPKV0,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.builder.{
  DualFundingTxFinalizer,
  RawTxBuilderWithFinalizer
}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.ECPublicKey

import scala.concurrent.{ExecutionContext, Future}

/** Responsible for constructing an unsigned DLC funding transaction
  * as well as all of its components (ScriptPubKeys, etc.)
  */
case class DLCFundingTxBuilder(
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
    acceptPayoutSPK: ScriptPubKey) {

  /** The total collateral of both parties combined */
  val totalInput: CurrencyUnit = offerInput + acceptInput

  /** The sum of all funding input amounts from the initiator */
  val offerTotalFunding: CurrencyUnit =
    offerFundingInputs.map(_.output.value).sum

  /** The sum of all funding input amounts from the non-initiator */
  val acceptTotalFunding: CurrencyUnit =
    acceptFundingInputs.map(_.output.value).sum

  require(
    offerTotalFunding >= offerInput,
    "Offer funding inputs must add up to at least offer's total collateral")
  require(
    acceptTotalFunding >= acceptInput,
    "Accept funding inputs must add up to at least accept's total collateral")

  private val fundingKeys =
    Vector(offerFundingKey, acceptFundingKey).sortBy(_.hex)

  /** The 2-of-2 MultiSignatureScriptPubKey to be wrapped in P2WSH and used as the funding output */
  val fundingMultiSig: MultiSignatureScriptPubKey =
    MultiSignatureScriptPubKey(2, fundingKeys)

  /** The funding output's P2WSH(MultiSig) ScriptPubKey */
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
