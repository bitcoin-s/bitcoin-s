package org.bitcoins.core.protocol.transaction

import grizzled.slf4j.Logging
import org.bitcoins.testkitcore.gen.TransactionGenerators
import org.scalacheck.{Prop, Properties}

/** Created by chris on 6/24/16.
  */
class TransactionInputSpec
    extends Properties("TransactionInputSpec")
    with Logging {

  property("Serialization symmetry") = {
    Prop.forAllNoShrink(TransactionGenerators.input) { input =>
      val result = TransactionInput(input.hex) == input
      result
    }
  }
}
