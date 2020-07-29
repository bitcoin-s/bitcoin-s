package org.bitcoins.dlc.builder

import org.bitcoins.commons.jsonmodels.dlc.DLCTimeouts
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.script.{
  EmptyScriptSignature,
  MultiSignatureScriptPubKey,
  P2WSHWitnessV0,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.builder.{
  AddWitnessDataFinalizer,
  P2WPKHSubtractFeesFromOutputsFinalizer,
  RawTxBuilder
}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{ConditionalPath, P2WSHV0InputInfo}
import org.bitcoins.crypto.ECPublicKey

import scala.concurrent.{ExecutionContext, Future}

/** Responsible for constructing the unsigned DLC refund transaction */
case class DLCRefundTxBuilder(
    offerInput: CurrencyUnit,
    offerFundingKey: ECPublicKey,
    offerFinalSPK: ScriptPubKey,
    acceptInput: CurrencyUnit,
    acceptFundingKey: ECPublicKey,
    acceptFinalSPK: ScriptPubKey,
    fundingOutputRef: OutputReference,
    timeouts: DLCTimeouts,
    feeRate: FeeUnit) {
  private val OutputReference(fundingOutPoint, fundingOutput) = fundingOutputRef
  private val totalInput = offerInput + acceptInput

  private val fundingInfo = P2WSHV0InputInfo(
    outPoint = fundingOutPoint,
    amount = fundingOutput.value,
    scriptWitness = P2WSHWitnessV0(
      MultiSignatureScriptPubKey(2, Vector(offerFundingKey, acceptFundingKey))),
    conditionalPath = ConditionalPath.NoCondition
  )

  /** Constructs the unsigned DLC refund transaction */
  def buildRefundTx()(implicit
      ec: ExecutionContext): Future[WitnessTransaction] = {
    val builder = RawTxBuilder().setLockTime(timeouts.contractTimeout.toUInt32)

    builder += TransactionInput(fundingOutPoint,
                                EmptyScriptSignature,
                                TransactionConstants.disableRBFSequence)

    val offerValue = Satoshis(
      (fundingOutput.value * offerInput).satoshis.toLong / totalInput.satoshis.toLong)
    val acceptValue = Satoshis(
      (fundingOutput.value * acceptInput).satoshis.toLong / totalInput.satoshis.toLong)

    builder += TransactionOutput(offerValue, offerFinalSPK)
    builder += TransactionOutput(acceptValue, acceptFinalSPK)

    // TODO: Should be SubtractFeeFromOutputsFinalizer since we know fundingInputInfo
    val finalizer = P2WPKHSubtractFeesFromOutputsFinalizer(
      feeRate,
      Vector(offerFinalSPK, acceptFinalSPK))
      .andThen(AddWitnessDataFinalizer(Vector(fundingInfo)))

    val txF = finalizer.buildTx(builder.result())

    txF.flatMap {
      case _: NonWitnessTransaction =>
        Future.failed(
          new RuntimeException(
            "Something went wrong with AddWitnessDataFinalizer"))
      case wtx: WitnessTransaction => Future.successful(wtx)
    }
  }
}
