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
  * @param actualFee fee paid by the transaction
  * @param expectedFee Fee rate the wallet expected to pay
  * @param feeRate Fee rate the transaction actually paid
  */
case class OutgoingTransactionDb(
    txIdBE: DoubleSha256DigestBE,
    inputAmount: CurrencyUnit,
    sentAmount: CurrencyUnit,
    actualFee: CurrencyUnit,
    expectedFee: CurrencyUnit,
    feeRate: SatoshisPerByte)
    extends TxDB {
  lazy val txId: DoubleSha256Digest = txIdBE.flip
}

object OutgoingTransactionDb {

  def fromTransaction(
      tx: Transaction,
      inputAmount: CurrencyUnit,
      sentAmount: CurrencyUnit,
      expectedFee: CurrencyUnit): OutgoingTransactionDb = {
    val totalOutput = tx.outputs.map(_.value).sum
    require(
      sentAmount <= totalOutput,
      s"sentAmount ($sentAmount) cannot be greater than the transaction's total output ($totalOutput)")
    require(
      sentAmount <= inputAmount,
      s"sentAmount ($sentAmount) cannot be greater than the amount the wallet input ($inputAmount)")

    val feePaid = inputAmount - totalOutput
    val feeRate = feePaid.satoshis.toLong / tx.baseSize.toDouble
    OutgoingTransactionDb(
      tx.txIdBE,
      inputAmount,
      sentAmount,
      feePaid,
      expectedFee,
      SatoshisPerByte.fromLong(feeRate.toLong)
    )
  }
}

class OutgoingTransactionTable(tag: Tag)
    extends Table[OutgoingTransactionDb](tag, "wallet_outgoing_txs")
    with TxTable[OutgoingTransactionDb] {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def txIdBE: Rep[DoubleSha256DigestBE] = column("txIdBE", O.Unique)

  def inputAmount: Rep[CurrencyUnit] = column("inputAmount")

  def sentAmount: Rep[CurrencyUnit] = column("sentAmount")

  def actualFee: Rep[CurrencyUnit] = column("actualFee")

  def expectedFee: Rep[CurrencyUnit] = column("expectedFee")

  def feeRate: Rep[SatoshisPerByte] = column("feeRate")

  private type OutgoingTransactionTuple =
    (
        DoubleSha256DigestBE,
        CurrencyUnit,
        CurrencyUnit,
        CurrencyUnit,
        CurrencyUnit,
        SatoshisPerByte)

  private val fromTuple: OutgoingTransactionTuple => OutgoingTransactionDb = {
    case (txId, inputAmount, sentAmount, actualFee, expectedFee, feeRate) =>
      OutgoingTransactionDb(txId,
                            inputAmount,
                            sentAmount,
                            actualFee,
                            expectedFee,
                            feeRate)
  }

  private val toTuple: OutgoingTransactionDb => Option[
    OutgoingTransactionTuple] = tx =>
    Some(
      (tx.txIdBE,
       tx.inputAmount,
       tx.sentAmount,
       tx.actualFee,
       tx.expectedFee,
       tx.feeRate))

  def * : ProvenShape[OutgoingTransactionDb] =
    (txIdBE, inputAmount, sentAmount, actualFee, expectedFee, feeRate) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey("pk_tx", sourceColumns = txIdBE)

  def fk_underlying_tx = {
    val txTable = TableQuery[TransactionTable]
    foreignKey("fk_underlying_tx",
               sourceColumns = txIdBE,
               targetTableQuery = txTable)(_.txIdBE)
  }

}
