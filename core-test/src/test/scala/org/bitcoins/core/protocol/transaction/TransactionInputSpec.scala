package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.testkitcore.gen.TransactionGenerators
import org.scalacheck.{Prop, Properties}

/** Created by chris on 6/24/16.
  */
class TransactionInputSpec
    extends Properties("TranactionInputSpec")
    with BitcoinSLogger {

  property("Serialization symmetry") = {
    Prop.forAllNoShrink(TransactionGenerators.input) { input =>
      val result = TransactionInput(input.hex) == input
      result
    }
  }
}
