package org.bitcoins.dlc.execution

import org.bitcoins.core.protocol.transaction.Transaction

sealed trait DLCOutcome {
  def fundingTx: Transaction
}

case class ExecutedDLCOutcome(
    override val fundingTx: Transaction,
    cet: Transaction)
    extends DLCOutcome

case class RefundDLCOutcome(
    override val fundingTx: Transaction,
    refundTx: Transaction)
    extends DLCOutcome
