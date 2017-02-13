package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.gen.TransactionGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/24/16.
  */
class TransactionInputSpec extends Properties("TranactionInputSpec") {

  property("Serialization symmetry") =
    Prop.forAll(TransactionGenerators.input) { input =>
      TransactionInput(input.hex) == input
    }
}
