package org.bitcoins.wallet.models

import org.bitcoins.core.protocol.transaction.Transaction
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape
import org.bitcoins.db.TableAutoInc
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.db.DbRowAutoInc
import org.bitcoins.core.protocol.script.ScriptPubKey

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
    confirmations: Int,
    id: Option[Long] = None
) extends TransactionDb[IncomingTransaction] {
  override def copyWithId(id: Long): IncomingTransaction = copy(id = Some(id))
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
  def scriptPubKey: Rep[ScriptPubKey] = column("our_script_pubkey")

  def fk_scriptPubKey =
    foreignKey("fk_script_pubkey",
               sourceColumns = scriptPubKey,
               targetTableQuery = TableQuery[AddressTable]) { addressTable =>
      addressTable.scriptPubKey
    }

  override def * : ProvenShape[IncomingTransaction] =
    (transaction, scriptPubKey, confirmations, id.?) <> (IncomingTransaction.tupled, IncomingTransaction.unapply)

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
