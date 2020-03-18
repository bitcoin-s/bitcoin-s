package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.EmptyScriptSignature
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{PrimaryKey, ProvenShape}

/** Represents a relevant transaction for the wallet that we should be keeping track of */
case class OutgoingTransactionDb(
    txIdBE: DoubleSha256DigestBE,
    transaction: Transaction,
    unsignedTxIdBE: DoubleSha256DigestBE,
    unsignedTx: Transaction,
    numInputs: Int,
    numOutputs: Int,
    lockTime: UInt32,
    txFee: CurrencyUnit,
    expectedFeeRate: FeeUnit,
    actualFeeRate: FeeUnit) {
  require(unsignedTx.inputs.forall(_.scriptSignature == EmptyScriptSignature),
          s"All ScriptSignatures must be empty, got $unsignedTx")

  lazy val txId: DoubleSha256Digest = txIdBE.flip
  lazy val unsignedTxId: DoubleSha256Digest = unsignedTxIdBE.flip
}

object OutgoingTransactionDb {

  def fromTransaction(
      tx: Transaction,
      expectedFeeRate: FeeUnit,
      inputAmount: CurrencyUnit): OutgoingTransactionDb = {
    val feePaid = inputAmount - tx.outputs.map(_.value).sum
    val feeRate = feePaid.satoshis.toLong / tx.vsize
    val unsignedTx = tx match {
      case btx: BaseTransaction =>
        val unsignedInputs = btx.inputs.map(
          input =>
            TransactionInput(input.previousOutput,
                             EmptyScriptSignature,
                             input.sequence))
        BaseTransaction(btx.version, unsignedInputs, btx.outputs, btx.lockTime)
      case wtx: WitnessTransaction =>
        WitnessTransaction(wtx.version,
                           wtx.inputs,
                           wtx.outputs,
                           wtx.lockTime,
                           EmptyWitness.fromInputs(wtx.inputs))
    }
    OutgoingTransactionDb(tx.txIdBE,
                          tx,
                          unsignedTx.txIdBE,
                          unsignedTx,
                          tx.inputs.size,
                          tx.outputs.size,
                          tx.lockTime,
                          feePaid,
                          expectedFeeRate,
                          SatoshisPerVirtualByte(feeRate))
  }
}

class OutgoingTransactionTable(tag: Tag)
    extends Table[OutgoingTransactionDb](tag, "wallet_outgoing_txs") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def txIdBE: Rep[DoubleSha256DigestBE] = column("txIdBE", O.Unique)

  def transaction: Rep[Transaction] = column("transaction")

  def unsignedTxIdBE: Rep[DoubleSha256DigestBE] = column("unsignedTxIdBE")

  def unsignedTx: Rep[Transaction] = column("unsignedTx")

  def numInputs: Rep[Int] = column("numInputs")

  def numOutputs: Rep[Int] = column("numOutputs")

  def locktime: Rep[UInt32] = column("locktime")

  def txFee: Rep[CurrencyUnit] = column("txFee")

  def expectedFeeRate: Rep[FeeUnit] = column("expectedFeeRate")

  def actualFeeRate: Rep[FeeUnit] = column("actualFeeRate")

  private type OutgoingTransactionTuple =
    (
        DoubleSha256DigestBE,
        Transaction,
        DoubleSha256DigestBE,
        Transaction,
        Int,
        Int,
        UInt32,
        CurrencyUnit,
        FeeUnit,
        FeeUnit)

  private val fromTuple: OutgoingTransactionTuple => OutgoingTransactionDb = {
    case (txId,
          transaction,
          unsignedTxIdBE,
          unsignedTx,
          numInputs,
          numOutputs,
          locktime,
          txFee,
          expectedFeeRate,
          actualFeeRate) =>
      OutgoingTransactionDb(txId,
                            transaction,
                            unsignedTxIdBE,
                            unsignedTx,
                            numInputs,
                            numOutputs,
                            locktime,
                            txFee,
                            expectedFeeRate,
                            actualFeeRate)
  }

  private val toTuple: OutgoingTransactionDb => Option[
    OutgoingTransactionTuple] = tx =>
    Some(
      (tx.txIdBE,
       tx.transaction,
       tx.unsignedTxIdBE,
       tx.unsignedTx,
       tx.numInputs,
       tx.numOutputs,
       tx.lockTime,
       tx.txFee,
       tx.expectedFeeRate,
       tx.actualFeeRate))

  def * : ProvenShape[OutgoingTransactionDb] =
    (txIdBE,
     transaction,
     unsignedTxIdBE,
     unsignedTx,
     numInputs,
     numOutputs,
     locktime,
     txFee,
     expectedFeeRate,
     actualFeeRate) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey("pk_tx", sourceColumns = txIdBE)

}
