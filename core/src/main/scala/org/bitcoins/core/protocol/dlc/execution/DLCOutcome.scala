package org.bitcoins.core.protocol.dlc.execution

import org.bitcoins.core.protocol.dlc.models.{OracleOutcome, OracleSignatures}
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}

sealed trait DLCOutcome {
  def fundingTx: Transaction
}

case class ExecutedDLCOutcome(
    override val fundingTx: Transaction,
    cet: WitnessTransaction,
    outcome: OracleOutcome,
    sigsUsed: Vector[OracleSignatures])
    extends DLCOutcome

case class RefundDLCOutcome(
    override val fundingTx: Transaction,
    refundTx: WitnessTransaction)
    extends DLCOutcome
