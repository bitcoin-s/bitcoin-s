package org.bitcoins.core.util

import org.bitcoins.core.gen.NumberGenerator
import org.bitcoins.core.number.{ UInt32, UInt8 }
import org.scalacheck.{ Gen, Prop, Properties }

/**
 * Created by chris on 6/20/16.
 */
class NumberUtilSpec extends Properties("NumberUtilSpec") {
  private val logger = BitcoinSLogger.logger

  property("Serialization symmetry for BigInt") =
    Prop.forAll(NumberGenerator.bigInts) { bigInt: BigInt =>
      NumberUtil.toBigInt(BitcoinSUtil.encodeHex(bigInt)) == bigInt
    }

  property("serialization symmetry for ints") =
    Prop.forAll { int: Int =>
      NumberUtil.toInt(BitcoinSUtil.encodeHex(int)) == int
    }

  property("serialization symmetry for longs") =
    Prop.forAll { long: Long =>
      NumberUtil.toLong(BitcoinSUtil.encodeHex(long)) == long
    }

  property("convertBits symmetry") = {
    Prop.forAllNoShrink(NumberGenerator.uInt8s) {
      case (u8s: Seq[UInt8]) =>
        val u5s = NumberUtil.convertUInt8sToUInt5s(u8s.toVector)
        val original: Vector[UInt8] = NumberUtil.convertUInt5sToUInt8(u5s = u5s)
        original == u8s
    }
  }
}
