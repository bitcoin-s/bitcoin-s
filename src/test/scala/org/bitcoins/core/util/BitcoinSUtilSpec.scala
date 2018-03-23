package org.bitcoins.core.util

import org.bitcoins.core.gen.{ CryptoGenerators, NumberGenerator, StringGenerators }
import org.scalacheck.{ Gen, Prop, Properties }
/**
 * Created by chris on 6/20/16.
 */
class BitcoinSUtilSpec extends Properties("BitcoinSUtilSpec") {

  property("Serialization symmetry for encodeHex & decodeHex") =
    Prop.forAll(StringGenerators.hexString) { hex: String =>
      BitcoinSUtil.encodeHex(BitcoinSUtil.decodeHex(hex)) == hex
    }

  property("Flipping endianness symmetry") =
    Prop.forAll(StringGenerators.hexString) { hex: String =>
      BitcoinSUtil.flipEndianness(BitcoinSUtil.flipEndianness(hex)) == hex
    }

  property("Convert a byte to a bit vector, convert it back to the original byte") =
    Prop.forAll { byte: Byte =>
      BitcoinSUtil.bitVectorToByte(BitcoinSUtil.byteToBitVector(byte)) == byte
    }

  property("Convert a sequence of bit vectors to a sequence of bytes") =
    Prop.forAll(NumberGenerator.bitVectors) { bitVectors: Seq[Seq[Boolean]] =>
      BitcoinSUtil.bytesToBitVectors(BitcoinSUtil.bitVectorsToBytes(bitVectors)) == bitVectors

    }
}
