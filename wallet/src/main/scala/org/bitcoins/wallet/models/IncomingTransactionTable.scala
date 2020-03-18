package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.EmptyScriptSignature
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  EmptyWitness,
  Transaction,
  TransactionInput,
  WitnessTransaction
}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{PrimaryKey, ProvenShape}

/** Represents a relevant transaction for the wallet that we should be keeping track of */
case class IncomingTransactionDb(
    txIdBE: DoubleSha256DigestBE,
    transaction: Transaction,
    unsignedTxIdBE: DoubleSha256DigestBE,
    unsignedTx: Transaction,
    numInputs: Int,
    numOutputs: Int,
    lockTime: UInt32) {
  require(unsignedTx.inputs.forall(_.scriptSignature == EmptyScriptSignature),
          s"All ScriptSignatures must be empty, got $unsignedTx")

  lazy val txId: DoubleSha256Digest = txIdBE.flip
  lazy val unsignedTxId: DoubleSha256Digest = unsignedTxIdBE.flip
}

object IncomingTransactionDb {

  def fromTransaction(tx: Transaction): IncomingTransactionDb = {
    val unsignedTx = tx match {
      case btx: BaseTransaction =>
        val unsignedInputs = btx.inputs.map(
          input =>
            TransactionInput(input.previousOutput,
                             EmptyScriptSignature,
                             input.sequence))
        BaseTransaction(btx.version, unsignedInputs, btx.outputs, btx.lockTime)
      case wtx: WitnessTransaction =>
        val unsignedInputs = wtx.inputs.map(
          input =>
            TransactionInput(input.previousOutput,
                             EmptyScriptSignature,
                             input.sequence))
        WitnessTransaction(wtx.version,
                           unsignedInputs,
                           wtx.outputs,
                           wtx.lockTime,
                           EmptyWitness.fromInputs(unsignedInputs))
    }
    IncomingTransactionDb(tx.txIdBE,
                          tx,
                          unsignedTx.txIdBE,
                          unsignedTx,
                          tx.inputs.size,
                          tx.outputs.size,
                          tx.lockTime)
  }
}

class IncomingTransactionTable(tag: Tag)
    extends Table[IncomingTransactionDb](tag, "wallet_incoming_txs") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def txIdBE: Rep[DoubleSha256DigestBE] = column("txIdBE", O.Unique)

  def transaction: Rep[Transaction] = column("transaction")

  def unsignedTxIdBE: Rep[DoubleSha256DigestBE] = column("unsignedTxIdBE")

  def unsignedTx: Rep[Transaction] = column("unsignedTx")

  def numInputs: Rep[Int] = column("numInputs")

  def numOutputs: Rep[Int] = column("numOutputs")

  def locktime: Rep[UInt32] = column("locktime")

  private type IncomingTransactionTuple =
    (
        DoubleSha256DigestBE,
        Transaction,
        DoubleSha256DigestBE,
        Transaction,
        Int,
        Int,
        UInt32)

  private val fromTuple: IncomingTransactionTuple => IncomingTransactionDb = {
    case (txId,
          transaction,
          unsignedTxIdBE,
          unsignedTx,
          numInputs,
          numOutputs,
          locktime) =>
      IncomingTransactionDb(txId,
                            transaction,
                            unsignedTxIdBE,
                            unsignedTx,
                            numInputs,
                            numOutputs,
                            locktime)
  }

  private val toTuple: IncomingTransactionDb => Option[
    IncomingTransactionTuple] = tx =>
    Some(
      (tx.txIdBE,
       tx.transaction,
       tx.unsignedTxIdBE,
       tx.unsignedTx,
       tx.numInputs,
       tx.numOutputs,
       tx.lockTime))

  def * : ProvenShape[IncomingTransactionDb] =
    (txIdBE,
     transaction,
     unsignedTxIdBE,
     unsignedTx,
     numInputs,
     numOutputs,
     locktime) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey("pk_tx", sourceColumns = txIdBE)

}
