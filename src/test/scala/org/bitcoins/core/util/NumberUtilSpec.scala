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

  property("converBits symmetry") = {
    Prop.forAllNoShrink(Gen.choose(1, 8), NumberGenerator.uInt8s) {
      case (to, u8s: Seq[UInt8]) =>
        //TODO: in the future make this a generated value instead of fixed to 8
        //but the trick is we need to make sure that the u8s generated are valid numbers in the 'from' base
        val u32From = UInt32(8.toShort)
        val u32To = UInt32(to.toShort)
        val converted = NumberUtil.convertUInt8s(u8s, u32From, u32To, true)
        val original = converted.flatMap(c => NumberUtil.convertUInt8s(c, u32To, u32From, false))
        if (original.isFailure) {
          throw original.failed.get
        } else {
          original.get == u8s
        }
    }
  }
}
