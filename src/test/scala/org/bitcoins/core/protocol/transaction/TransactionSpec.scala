package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/24/16.
  */
class TransactionSpec extends Properties("TransactionSpec") with BitcoinSLogger {

  property("Serialization symmetry") =
    Prop.forAll(TransactionGenerators.transactions) { tx =>
      logger.info("Transaction: " + tx)
      logger.info("Tx hex: " + tx.hex)
      Transaction(tx.hex) == tx

    }
}
