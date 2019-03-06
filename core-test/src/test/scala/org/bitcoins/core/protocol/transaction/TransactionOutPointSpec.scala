package org.bitcoins.core.protocol.transaction

import org.bitcoins.testkit.core.gen.TransactionGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/21/16.
  */
class TransactionOutPointSpec extends Properties("TransactionOutPointSpec") {

  property("Serialization symmetry") =
    Prop.forAll(TransactionGenerators.outPoint) { outPoint =>
      TransactionOutPoint(outPoint.hex) == outPoint
    }
}
