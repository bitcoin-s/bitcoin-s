package org.bitcoins.core.util

import org.bitcoins.core.gen.StringGenerators
import org.scalacheck.{Gen, Prop, Properties}
/**
  * Created by chris on 6/20/16.
  */
class BitcoinSUtilSpec extends Properties("BitcoinSUtilSpec") with BitcoinSLogger {

  property("Serialization symmetry for encodeHex & decodeHex") =
    Prop.forAll(StringGenerators.hexString) { hex : String =>
      BitcoinSUtil.encodeHex(BitcoinSUtil.decodeHex(hex)) == hex
    }

  property("Flipping endianess symmetry") =
    Prop.forAll(StringGenerators.hexString) { hex : String =>
      BitcoinSUtil.flipEndianess(BitcoinSUtil.flipEndianess(hex)) == hex
    }
}
