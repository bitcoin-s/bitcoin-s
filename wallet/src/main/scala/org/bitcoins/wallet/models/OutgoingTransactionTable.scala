package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{PrimaryKey, ProvenShape}

/**
  * Represents a relevant transaction for the wallet that we should be keeping track of
  * @param txIdBE Transaction ID
  * @param txFee fee paid by the transaction
  * @param expectedFeeRate Fee rate the wallet expected to pay
  * @param actualFeeRate Fee rate the transaction actually paid
  */
case class OutgoingTransactionDb(
    txIdBE: DoubleSha256DigestBE,
    txFee: CurrencyUnit,
    expectedFeeRate: SatoshisPerByte,
    actualFeeRate: SatoshisPerByte) {
  lazy val txId: DoubleSha256Digest = txIdBE.flip
}

object OutgoingTransactionDb {

  def fromTransaction(
      tx: Transaction,
      expectedFeeRate: SatoshisPerByte,
      inputAmount: CurrencyUnit): OutgoingTransactionDb = {
    val feePaid = inputAmount - tx.outputs.map(_.value).sum
    val feeRate = feePaid.satoshis.toLong / tx.baseSize
    OutgoingTransactionDb(
      tx.txIdBE,
      feePaid,
      expectedFeeRate,
      SatoshisPerByte.fromLong(feeRate)
    )
  }
}

class OutgoingTransactionTable(tag: Tag)
    extends Table[OutgoingTransactionDb](tag, "wallet_outgoing_txs") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def txIdBE: Rep[DoubleSha256DigestBE] = column("txIdBE", O.Unique)

  def txFee: Rep[CurrencyUnit] = column("txFee")

  def expectedFeeRate: Rep[SatoshisPerByte] = column("expectedFeeRate")

  def actualFeeRate: Rep[SatoshisPerByte] = column("actualFeeRate")

  private type OutgoingTransactionTuple =
    (DoubleSha256DigestBE, CurrencyUnit, SatoshisPerByte, SatoshisPerByte)

  private val fromTuple: OutgoingTransactionTuple => OutgoingTransactionDb = {
    case (txId, txFee, expectedFeeRate, actualFeeRate) =>
      OutgoingTransactionDb(txId, txFee, expectedFeeRate, actualFeeRate)
  }

  private val toTuple: OutgoingTransactionDb => Option[
    OutgoingTransactionTuple] = tx =>
    Some((tx.txIdBE, tx.txFee, tx.expectedFeeRate, tx.actualFeeRate))

  def * : ProvenShape[OutgoingTransactionDb] =
    (txIdBE, txFee, expectedFeeRate, actualFeeRate) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey("pk_tx", sourceColumns = txIdBE)

  def fk_txId = {
    val txTable = TableQuery[TransactionTable]
    foreignKey("fk_txId", sourceColumns = txIdBE, targetTableQuery = txTable)(
      _.txIdBE)
  }

}
