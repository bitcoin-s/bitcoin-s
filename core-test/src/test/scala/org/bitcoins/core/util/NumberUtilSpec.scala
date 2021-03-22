package org.bitcoins.core.util

import grizzled.slf4j.Logging
import org.bitcoins.core.number.UInt8
import org.bitcoins.testkitcore.gen.NumberGenerator
import org.scalacheck.{Prop, Properties}

/** Created by chris on 6/20/16.
  */
class NumberUtilSpec extends Properties("NumberUtilSpec") with Logging {

  property("Serialization symmetry for BigInt") =
    Prop.forAll(NumberGenerator.bigInts) { bigInt: BigInt =>
      NumberUtil.toBigInt(BytesUtil.encodeHex(bigInt)) == bigInt
    }

  property("serialization symmetry for ints") = Prop.forAll { int: Int =>
    NumberUtil.toInt(BytesUtil.encodeHex(int)) == int
  }

  property("serialization symmetry for longs") = Prop.forAll { long: Long =>
    NumberUtil.toLong(BytesUtil.encodeHex(long)) == long
  }

  property("convertBits symmetry") = {
    Prop.forAllNoShrink(NumberGenerator.uInt8s) { case (u8s: Seq[UInt8]) =>
      val u5s = NumberUtil.convertUInt8sToUInt5s(u8s.toVector)
      val original: Vector[UInt8] = NumberUtil.convertUInt5sToUInt8(u5s = u5s)
      original == u8s
    }
  }
}
