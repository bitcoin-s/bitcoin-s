package org.bitcoins.dlc.execution

import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}

sealed trait DLCOutcome {
  def fundingTx: Transaction
}

sealed trait UnilateralDLCOutcome extends DLCOutcome {
  def cet: Transaction
}

/** Contains all DLC transactions and the BitcoinUTXOSpendingInfos they use. */
case class UnilateralDLCOutcomeWithClosing(
    override val fundingTx: Transaction,
    override val cet: Transaction,
    closingTx: Transaction,
    cetSpendingInfo: ScriptSignatureParams[InputInfo]
) extends UnilateralDLCOutcome

case class UnilateralDLCOutcomeWithDustClosing(
    override val fundingTx: Transaction,
    override val cet: Transaction)
    extends UnilateralDLCOutcome

sealed trait RefundDLCOutcome extends DLCOutcome {
  def refundTx: Transaction
}

case class RefundDLCOutcomeWithClosing(
    override val fundingTx: Transaction,
    override val refundTx: Transaction,
    closingTx: Transaction,
    refundSpendingInfo: ScriptSignatureParams[InputInfo])
    extends RefundDLCOutcome

case class RefundDLCOutcomeWithDustClosing(
    override val fundingTx: Transaction,
    override val refundTx: Transaction)
    extends RefundDLCOutcome

case class CooperativeDLCOutcome(
    override val fundingTx: Transaction,
    closingTx: Transaction)
    extends DLCOutcome
