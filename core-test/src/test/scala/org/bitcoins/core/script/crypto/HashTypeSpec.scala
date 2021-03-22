package org.bitcoins.core.script.crypto

import grizzled.slf4j.Logging
import org.bitcoins.testkitcore.gen.NumberGenerator
import org.scalacheck.{Prop, Properties}

class HashTypeSpec extends Properties("HashTypeSpec") with Logging {

  property("serialization symmetry") = {
    Prop.forAll(NumberGenerator.int32s) { i32 =>
      val hashType = HashType.fromBytes(i32.bytes)

      hashType.num == i32 &&
      i32.bytes.last == hashType.byte &&
      //this check cannot check the other 3 bytes in
      //hash type as they are discarded from inclusion
      //on a bitcoin digital signature. Not sure why satoshi
      //would have just used a uint8_t to represent a hash type
      //instead of a uint32_t.
      HashType.fromByte(hashType.byte).byte == hashType.byte

    }
  }
}
