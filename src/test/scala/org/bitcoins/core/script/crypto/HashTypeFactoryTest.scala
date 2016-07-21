package org.bitcoins.core.script.crypto

import org.bitcoins.core.number.Int32
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/18/16.
 */
class HashTypeFactoryTest extends FlatSpec with MustMatchers  {

  "HashTypeFactory" must "find a hash type by its hex value" in {
    HashTypeFactory("01") must be (SIGHASH_ALL())
    HashTypeFactory("02") must be (SIGHASH_NONE)
    HashTypeFactory("03") must be (SIGHASH_SINGLE)
    HashTypeFactory("80") must be (SIGHASH_ANYONECANPAY)
  }

  it must "find a hash type by its byte value" in {

    HashTypeFactory.fromBytes(Seq(0x01)) must be (SIGHASH_ALL())
    HashTypeFactory.fromBytes(Seq(0x02)) must be (SIGHASH_NONE)
    HashTypeFactory.fromBytes(Seq(0x03)) must be (SIGHASH_SINGLE)
    HashTypeFactory.fromBytes(Seq(0x80.toByte)) must be (SIGHASH_ANYONECANPAY)
  }

  it must "default to SIGHASH_ALL if the given string/byte is not known" in {
    HashTypeFactory.fromBytes(Seq(0x96.toByte)) must be (SIGHASH_ALL(Int32(-106)))
  }


}
