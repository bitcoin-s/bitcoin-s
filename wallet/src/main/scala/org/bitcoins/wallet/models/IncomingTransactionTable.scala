package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{PrimaryKey, ProvenShape}

/**
  * Represents a relevant transaction for the wallet that we should be keeping track of
  * @param txIdBE Transaction ID
  */
case class IncomingTransactionDb(
    txIdBE: DoubleSha256DigestBE,
    incomingAmount: CurrencyUnit)
    extends TxDB {
  lazy val txId: DoubleSha256Digest = txIdBE.flip
}

class IncomingTransactionTable(tag: Tag)
    extends Table[IncomingTransactionDb](tag, "wallet_incoming_txs")
    with TxTable[IncomingTransactionDb] {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def txIdBE: Rep[DoubleSha256DigestBE] = column("txIdBE", O.Unique)

  def incomingAmount: Rep[CurrencyUnit] = column("incomingAmount")

  private type IncomingTransactionTuple = (DoubleSha256DigestBE, CurrencyUnit)

  private val fromTuple: IncomingTransactionTuple => IncomingTransactionDb = {
    case (txId, incomingAmount) =>
      IncomingTransactionDb(txId, incomingAmount)
  }

  private val toTuple: IncomingTransactionDb => Option[
    IncomingTransactionTuple] = tx => Some((tx.txIdBE, tx.incomingAmount))

  def * : ProvenShape[IncomingTransactionDb] =
    (txIdBE, incomingAmount) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey("pk_tx", sourceColumns = txIdBE)

  def fk_underlying_tx = {
    val txTable = TableQuery[TransactionTable]
    foreignKey("fk_underlying_tx",
               sourceColumns = txIdBE,
               targetTableQuery = txTable)(_.txIdBE)
  }
}
