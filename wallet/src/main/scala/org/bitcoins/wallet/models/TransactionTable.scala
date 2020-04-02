package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
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

trait TxDB {
  def txIdBE: DoubleSha256DigestBE
}

trait TxTable[DbEntryType <: TxDB] extends Table[DbEntryType] {
  def txIdBE: Rep[DoubleSha256DigestBE]
}

/**
  * Represents a relevant transaction for the wallet that we should be keeping track of
  * @param txIdBE Transaction ID
  * @param transaction Serialized Transaction
  * @param unsignedTxIdBE Transaction ID of the unsigned transaction
  * @param unsignedTx Unsigned Transaction. This is useful so we can reconcile what our estimated
  *                   fees were against our actual fees in the case of ECDSA signature size variability
  * @param wTxIdBEOpt Witness Transaction ID
  * @param numInputs Number of inputs in the transaction
  * @param numOutputs Number of outputs in the transaction
  * @param lockTime locktime of the transaction
  */
case class TransactionDb(
    txIdBE: DoubleSha256DigestBE,
    transaction: Transaction,
    unsignedTxIdBE: DoubleSha256DigestBE,
    unsignedTx: Transaction,
    wTxIdBEOpt: Option[DoubleSha256DigestBE],
    totalOutput: CurrencyUnit,
    numInputs: Int,
    numOutputs: Int,
    lockTime: UInt32)
    extends TxDB {
  require(unsignedTx.inputs.forall(_.scriptSignature == EmptyScriptSignature),
          s"All ScriptSignatures must be empty, got $unsignedTx")

  lazy val txId: DoubleSha256Digest = txIdBE.flip
  lazy val unsignedTxId: DoubleSha256Digest = unsignedTxIdBE.flip
  lazy val wTxIdOpt: Option[DoubleSha256Digest] = wTxIdBEOpt.map(_.flip)
}

object TransactionDb {

  def fromTransaction(tx: Transaction): TransactionDb = {
    val (unsignedTx, wTxIdBEOpt) = tx match {
      case btx: BaseTransaction =>
        val unsignedInputs = btx.inputs.map(
          input =>
            TransactionInput(input.previousOutput,
                             EmptyScriptSignature,
                             input.sequence))
        (BaseTransaction(btx.version,
                         unsignedInputs,
                         btx.outputs,
                         btx.lockTime),
         None)
      case wtx: WitnessTransaction =>
        val unsignedInputs = wtx.inputs.map(
          input =>
            TransactionInput(input.previousOutput,
                             EmptyScriptSignature,
                             input.sequence))
        val uwtx = WitnessTransaction(wtx.version,
                                      unsignedInputs,
                                      wtx.outputs,
                                      wtx.lockTime,
                                      EmptyWitness.fromInputs(unsignedInputs))

        (uwtx, Some(uwtx.wTxIdBE))
    }
    val totalOutput = tx.outputs.map(_.value).sum
    TransactionDb(tx.txIdBE,
                  tx,
                  unsignedTx.txIdBE,
                  unsignedTx,
                  wTxIdBEOpt,
                  totalOutput,
                  tx.inputs.size,
                  tx.outputs.size,
                  tx.lockTime)
  }
}

class TransactionTable(tag: Tag)
    extends Table[TransactionDb](tag, "tx_table")
    with TxTable[TransactionDb] {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def txIdBE: Rep[DoubleSha256DigestBE] = column("txIdBE", O.Unique)

  def transaction: Rep[Transaction] = column("transaction")

  def unsignedTxIdBE: Rep[DoubleSha256DigestBE] = column("unsignedTxIdBE")

  def unsignedTx: Rep[Transaction] = column("unsignedTx")

  def wTxIdBEOpt: Rep[Option[DoubleSha256DigestBE]] =
    column("wTxIdBE")

  def totalOutput: Rep[CurrencyUnit] = column("totalOutput")

  def numInputs: Rep[Int] = column("numInputs")

  def numOutputs: Rep[Int] = column("numOutputs")

  def locktime: Rep[UInt32] = column("locktime")

  private type TransactionTuple =
    (
        DoubleSha256DigestBE,
        Transaction,
        DoubleSha256DigestBE,
        Transaction,
        Option[DoubleSha256DigestBE],
        CurrencyUnit,
        Int,
        Int,
        UInt32)

  private val fromTuple: TransactionTuple => TransactionDb = {
    case (txId,
          transaction,
          unsignedTxIdBE,
          unsignedTx,
          wTxIdBEOpt,
          totalOutput,
          numInputs,
          numOutputs,
          locktime) =>
      TransactionDb(txId,
                    transaction,
                    unsignedTxIdBE,
                    unsignedTx,
                    wTxIdBEOpt,
                    totalOutput,
                    numInputs,
                    numOutputs,
                    locktime)
  }

  private val toTuple: TransactionDb => Option[TransactionTuple] = tx =>
    Some(
      (tx.txIdBE,
       tx.transaction,
       tx.unsignedTxIdBE,
       tx.unsignedTx,
       tx.wTxIdBEOpt,
       tx.totalOutput,
       tx.numInputs,
       tx.numOutputs,
       tx.lockTime))

  def * : ProvenShape[TransactionDb] =
    (txIdBE,
     transaction,
     unsignedTxIdBE,
     unsignedTx,
     wTxIdBEOpt,
     totalOutput,
     numInputs,
     numOutputs,
     locktime) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey("pk_tx", sourceColumns = txIdBE)

}
