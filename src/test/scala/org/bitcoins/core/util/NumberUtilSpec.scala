package org.bitcoins.core.util

import org.bitcoins.core.gen.NumberGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/20/16.
  */
class NumberUtilSpec extends Properties("NumberUtilSpec") {

  property("Serialization symmetry for BigInt") =
    Prop.forAll(NumberGenerator.bigInts) { bigInt : BigInt =>
      NumberUtil.toBigInt(BitcoinSUtil.encodeHex(bigInt)) == bigInt
    }

  property("serialization symmetry for ints") =
    Prop.forAll { int : Int =>
      NumberUtil.toInt(BitcoinSUtil.encodeHex(int)) == int
    }

  property("serialization symmetry for longs") =
    Prop.forAll { long : Long =>
      NumberUtil.toLong(BitcoinSUtil.encodeHex(long)) == long
    }
}
