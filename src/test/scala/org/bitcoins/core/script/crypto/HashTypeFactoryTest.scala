package org.bitcoins.core.script.crypto

import org.bitcoins.core.number.Int32
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/18/16.
 */
class HashTypeFactoryTest extends FlatSpec with MustMatchers  {

  "HashTypeFactory" must "find a hash type by its hex value" in {
    HashTypeFactory("00000001") must be (SIGHASH_ALL(Int32.one))
    HashTypeFactory("00000002") must be (SIGHASH_NONE)
    HashTypeFactory("00000003") must be (SIGHASH_SINGLE)
    HashTypeFactory("00000080") must be (SIGHASH_ANYONECANPAY)
  }

  it must "find a hash type by its byte value" in {

    HashTypeFactory.fromBytes(Seq(0.toByte)) must be (SIGHASH_ALL(Int32.zero))
    HashTypeFactory.fromBytes(Seq(1.toByte)) must be (SIGHASH_ALL(Int32.one))
    HashTypeFactory.fromBytes(Seq(2.toByte)) must be (SIGHASH_NONE)
    HashTypeFactory.fromBytes(Seq(3.toByte)) must be (SIGHASH_SINGLE)
    HashTypeFactory.fromBytes(Int32(0x80).bytes) must be (SIGHASH_ANYONECANPAY)
  }

  it must "default to SIGHASH_ALL if the given string/byte is not known" in {
    HashTypeFactory.fromBytes(Seq(0x124.toByte)) must be (SIGHASH_ALL(Int32(36)))
  }

  it must "find hashType for number 1190874345" in {
    //1190874345 & 0x1f = 0x80
    val num = Int32(1190874345)
    HashTypeFactory.fromNumber(num) must be (SIGHASH_ANYONECANPAY)
  }
}
