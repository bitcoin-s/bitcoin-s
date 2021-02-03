package org.bitcoins.core.util

import org.bitcoins.testkit.core.gen.StringGenerators
import org.scalacheck.{Prop, Properties}

/** Created by chris on 6/20/16.
  */
class BitcoinSUtilSpec extends Properties("BitcoinSUtilSpec") {

  property("Serialization symmetry for encodeHex & decodeHex") =
    Prop.forAll(StringGenerators.hexString) { hex: String =>
      BytesUtil.encodeHex(BytesUtil.decodeHex(hex)) == hex
    }

  property("Flipping endianness symmetry") =
    Prop.forAll(StringGenerators.hexString) { hex: String =>
      BytesUtil.flipEndianness(BytesUtil.flipEndianness(hex)) == hex
    }

  property(
    "Convert a byte to a bit vector, convert it back to the original byte") =
    Prop.forAll { byte: Byte =>
      BytesUtil
        .bitVectorToBytes(BytesUtil.byteToBitVector(byte))
        .toByte() == byte
    }
}
