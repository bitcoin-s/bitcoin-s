package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.gen.TransactionGenerators
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/24/16.
  */
class TransactionSpec extends Properties("TransactionSpec") with BitcoinSLogger {

  property("Serialization symmetry") =
    
    Prop.forAll(TransactionGenerators.transactions) { tx =>
      val result = Transaction(tx.hex) == tx
      if (!result) logger.error("Incorrect tx hex: " + tx.hex)
      result
    }

  property("Serialization symmetry for witness transactions") =
    Prop.forAll(TransactionGenerators.witnessTransaction) { wtx: WitnessTransaction =>
      val result = WitnessTransaction(wtx.hex) == wtx
      if (!result) logger.error("Incorrect wtx hex: " + wtx.hex)
      result
    }
}
