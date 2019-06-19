package org.bitcoins.wallet.models

import org.bitcoins.core.protocol.transaction.Transaction
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape
import org.bitcoins.db.TableAutoInc
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.db.DbRowAutoInc
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutput

/**
  * Database representation of transactions
  * relevant to our wallet.
  */
sealed trait TransactionDb[T <: TransactionDb[_]] extends DbRowAutoInc[T] {
  val transaction: Transaction
  lazy val txid: DoubleSha256DigestBE = transaction.txIdBE
  val confirmations: Int

}

/** Transactions our wallet has received */
final case class IncomingTransaction(
    transaction: Transaction,
    scriptPubKey: ScriptPubKey,
    voutIndex: Int,
    confirmations: Int,
    id: Option[Long] = None
) extends TransactionDb[IncomingTransaction] {
  require(voutIndex >= 0, s"voutIndex cannot be negative, got $voutIndex")
  override def copyWithId(id: Long): IncomingTransaction = copy(id = Some(id))

  /** The output we're interested in
    *
    * TODO: Concerns about TXs paying to multiple SPKs in our wallet, see note below
    */
  lazy val output: TransactionOutput = transaction.outputs(voutIndex)
}

/** Transactions our wallet has sent */
final case class OutgoingTransaction(
    transaction: Transaction,
    confirmations: Int,
    id: Option[Long] = None,
    utxoId: Option[Long] = None
) extends TransactionDb[OutgoingTransaction] {
  override def copyWithId(id: Long): OutgoingTransaction = copy(id = Some(id))
}

sealed abstract class TransactionTable[TxType <: TransactionDb[_]](
    tag: Tag,
    tableName: String)
    extends TableAutoInc[TxType](tag, tableName) {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def transaction: Rep[Transaction] = column("transaction")

  def confirmations: Rep[Int] = column("confirmations")

}

final case class IncomingTransactionTable(tag: Tag)
    extends TransactionTable[IncomingTransaction](tag, "incoming_transactions") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  // TODO: What happens if we get paid to multiple SPKs in the same
  // transaction? Need to make a table of SPKs, and map IDs in that
  // table to TXs in this table...
  /** The SPK that's relevant to us in this transaction. Foreign key into address table */
  def scriptPubKey: Rep[ScriptPubKey] = column("our_script_pubkey")

  // TODO: The same concerns as above
  /** The output of this TX that's ours */
  def voutIndex: Rep[Int] = column("vout_index")

  def fk_scriptPubKey =
    foreignKey("fk_script_pubkey",
               sourceColumns = scriptPubKey,
               targetTableQuery = TableQuery[AddressTable]) { addressTable =>
      addressTable.scriptPubKey
    }

  override def * : ProvenShape[IncomingTransaction] =
    (transaction, scriptPubKey, confirmations, voutIndex, id.?) <> (IncomingTransaction.tupled, IncomingTransaction.unapply)

}

final case class OutgoingTransactionTable(tag: Tag)
    extends TransactionTable[OutgoingTransaction](tag, "outgoing_transactions") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def utxoId: Rep[Long] = column("utxo_id", O.Unique)

  def fk_utxo = {
    val utxoTable = TableQuery[UTXOSpendingInfoTable]
    foreignKey("fk_utxo", sourceColumns = utxoId, targetTableQuery = utxoTable) {
      _.id
    }
  }

  override def * : ProvenShape[OutgoingTransaction] =
    (transaction, confirmations, id.?, utxoId.?) <> (OutgoingTransaction.tupled, OutgoingTransaction.unapply)

}
