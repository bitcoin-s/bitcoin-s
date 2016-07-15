package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.gen.TransactionGenerators
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/21/16.
  */
class TransactionOutPointSpec extends Properties("TransactionOutPointSpec") with BitcoinSLogger {

  property("Serialization symmetry") =
    Prop.forAll(TransactionGenerators.outPoints) { outPoint =>
      TransactionOutPoint(outPoint.hex) == outPoint
    }
}
