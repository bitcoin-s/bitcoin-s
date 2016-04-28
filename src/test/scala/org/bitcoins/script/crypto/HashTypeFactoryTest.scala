package org.bitcoins.script.crypto

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/18/16.
 */
class HashTypeFactoryTest extends FlatSpec with MustMatchers  {

  "HashTypeFactory" must "find a hash type by its hex value" in {
    HashTypeFactory.fromString("01") must be (SIGHASH_ALL())
    HashTypeFactory.fromString("02") must be (SIGHASH_NONE)
    HashTypeFactory.fromString("03") must be (SIGHASH_SINGLE)
    HashTypeFactory.fromString("80") must be (SIGHASH_ANYONECANPAY)
  }

  it must "find a hash type by its byte value" in {

    HashTypeFactory.fromByte(0x01) must be (SIGHASH_ALL())
    HashTypeFactory.fromByte(0x02) must be (SIGHASH_NONE)
    HashTypeFactory.fromByte(0x03) must be (SIGHASH_SINGLE)
    HashTypeFactory.fromByte(0x80.toByte) must be (SIGHASH_ANYONECANPAY)
  }

  it must "default to SIGHASH_ALL if the given string/byte is not known" in {
    HashTypeFactory.fromByte(0x96.toByte) must be (SIGHASH_ALL(-106))
  }


}
