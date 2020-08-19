package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.TxDB
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}

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
