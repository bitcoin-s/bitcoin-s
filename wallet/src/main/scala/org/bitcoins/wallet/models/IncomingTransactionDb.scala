package org.bitcoins.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}

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
