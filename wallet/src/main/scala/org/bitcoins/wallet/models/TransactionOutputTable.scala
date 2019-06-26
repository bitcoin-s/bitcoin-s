package org.bitcoins.wallet.models

import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape
import org.bitcoins.db.TableAutoInc
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.db.DbRowAutoInc
import org.bitcoins.core.protocol.script.ScriptPubKey

/**
  * A transaction output that's relevant to our wallet
  */
sealed trait WalletTXO[T <: WalletTXO[T]] extends DbRowAutoInc[T] {
  require(confirmations >= 0,
          s"Confirmations cannot be negative! Got: $confirmations")

  /** The transaction that this output was
    * received/spent in
    */
  val txid: DoubleSha256DigestBE

  /** Whether or not this TXO is spent */
  val spent: Boolean

  /** How many confirmations this TXO has */
  val confirmations: Int

}

/**
  * A transaction output that has been spent to our wallet
  * @param spendingInfoID Foreign key into the table with
  *                       TXO spending info
  * @param id
  */
final case class IncomingWalletTXO(
    confirmations: Int,
    txid: DoubleSha256DigestBE,
    spent: Boolean,
    scriptPubKey: ScriptPubKey,
    spendingInfoID: Long,
    id: Option[Long] = None)
    extends WalletTXO[IncomingWalletTXO] {
  override def copyWithId(id: Long): IncomingWalletTXO =
    this.copy(id = Some(id))

}

final case class OutgoingWalletTXO(
    confirmations: Int,
    txid: DoubleSha256DigestBE,
    incomingTxoID: Long,
    id: Option[Long] = None)
    extends WalletTXO[OutgoingWalletTXO] {

  override def copyWithId(id: Long): OutgoingWalletTXO = copy(id = Some(id))

  /** Outgoing TXOs are per definition spent */
  val spent: Boolean = true

}

/**
  * Table of outputs relevant to our wallet, somehow.
  *
  * This table does not contain information related to spending
  * the TXO, that's handled in SpendingInfoTable
  */
sealed abstract class WalletTXOTable[TXOType <: WalletTXO[TXOType]](
    tag: Tag,
    tableName: String)
    extends TableAutoInc[TXOType](tag, tableName) {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def txid: Rep[DoubleSha256DigestBE] = column("txid")
  def confirmations: Rep[Int] = column("confirmations")
  def spent: Rep[Boolean] = column("spent")
}

final case class IncomingTXOTable(tag: Tag)
    extends WalletTXOTable[IncomingWalletTXO](tag, "incoming_tx_outputs") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def scriptPubKey: Rep[ScriptPubKey] = column("script_pub_key")

  def spendingInfoID: Rep[Long] = column("spending_info_id")

  /** All incoming TXOs must have a SPK that gets spent to */
  def fk_scriptPubKey = {
    val addressTable = TableQuery[AddressTable]
    foreignKey("fk_scriptPubKey",
               sourceColumns = scriptPubKey,
               targetTableQuery = addressTable)(_.scriptPubKey)
  }

  /**
    * Every incoming TXO must be spendable, this foreign key ensures
    * this
    */
  def fk_spendingInfo = {
    val spendingInfoTable = TableQuery[SpendingInfoTable]
    foreignKey("fk_spending_info",
               sourceColumns = spendingInfoID,
               targetTableQuery = spendingInfoTable)(_.id)
  }

  override def * : ProvenShape[IncomingWalletTXO] =
    (confirmations, txid, spent, scriptPubKey, spendingInfoID, id.?) <> (IncomingWalletTXO.tupled, IncomingWalletTXO.unapply)
}

final case class OutgoingTXOTable(tag: Tag)
    extends WalletTXOTable[OutgoingWalletTXO](tag, "outgoing_tx_outputs") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  /** Every outgoing TXO must reference how it entered our wallet */
  def incomingTxoID: Rep[Long] = column("incoming_txo_id")

  def fk_incoming = {
    val incomingTable = TableQuery[IncomingTXOTable]
    foreignKey("fk_incoming_txo",
               sourceColumns = incomingTxoID,
               targetTableQuery = incomingTable) { _.id }
  }

  override def * : ProvenShape[OutgoingWalletTXO] =
    (confirmations, txid, incomingTxoID, id.?) <> (OutgoingWalletTXO.tupled, OutgoingWalletTXO.unapply)
}
