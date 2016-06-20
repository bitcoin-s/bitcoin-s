package org.bitcoins.core.util

import org.bitcoins.core.number.NumberGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/20/16.
  */
class NumberUtilSpecification extends Properties("NumberUtilSpec") {

  property("Serialization symmetry for BigInt") =
    Prop.forAll(NumberGenerator.bigInts) { bigInt : BigInt =>
      NumberUtil.toBigInt(BitcoinSUtil.encodeHex(bigInt)) == bigInt
    }

}
