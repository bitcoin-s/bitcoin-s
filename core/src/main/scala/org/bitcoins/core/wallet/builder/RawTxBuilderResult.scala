package org.bitcoins.core.wallet.builder

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  Transaction,
  TransactionInput,
  TransactionOutput
}

/** Raw Transaction to be finalized by a RawTxFinalizer */
case class RawTxBuilderResult(
    version: Int32,
    inputs: Vector[TransactionInput],
    outputs: Vector[TransactionOutput],
    lockTime: UInt32) {

  def toBaseTransaction: BaseTransaction = {
    BaseTransaction(version, inputs, outputs, lockTime)
  }
}

object RawTxBuilderResult {

  def fromTransaction(tx: Transaction): RawTxBuilderResult = {
    RawTxBuilderResult(tx.version,
                       tx.inputs.toVector,
                       tx.outputs.toVector,
                       tx.lockTime)
  }
}
