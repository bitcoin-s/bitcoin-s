package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.gen.WitnessGenerators
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 11/28/16.
  */
class TransactionInputWitnessSpec extends Properties("TransactionInputWitnessSpec") with BitcoinSLogger {

  property("Serialization symmetry") = {
    Prop.forAll(WitnessGenerators.transactionInputWitness) { inputWitness: TransactionInputWitness =>
      logger.debug("Original hex: " + inputWitness.hex)
      TransactionInputWitness(inputWitness.hex) == inputWitness
    }
  }
}
