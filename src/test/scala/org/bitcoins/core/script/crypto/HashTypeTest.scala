package org.bitcoins.core.script.crypto

import org.bitcoins.core.number.Int32
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/27/16.
 */
class HashTypeTest extends FlatSpec with MustMatchers {

  "HashType" must "combine hash types with SIGHASH_ANYONECANPAY" in {
    SIGHASH_ALL_ANYONECANPAY.value.num must be (Int32(0x81))
    SIGHASH_NONE_ANYONECANPAY.value.num must be (Int32(0x82))
    SIGHASH_SINGLE_ANYONECANPAY.value.num must be (Int32(0x83))
  }

  it must "find a hash type by its hex value" in {
    HashTypeOperations("00000001") must be (SIGHASH_ALL.value)
    HashTypeOperations("00000002") must be (SIGHASH_NONE.value)
    HashTypeOperations("00000003") must be (SIGHASH_SINGLE.value)
    HashTypeOperations("00000080") must be (SIGHASH_ANYONECANPAY.value)
  }

  it must "find a hash type by its byte value" in {

    HashTypeOperations(Seq(0.toByte)) must be (SIGHASH_ALL(Int32.zero))
    HashTypeOperations(Seq(1.toByte)) must be (SIGHASH_ALL(Int32.one))
    HashTypeOperations(Seq(2.toByte)) must be (SIGHASH_NONE.value)
    HashTypeOperations(Seq(3.toByte)) must be (SIGHASH_SINGLE.value)
    HashTypeOperations(Int32(0x80).bytes) must be (SIGHASH_ANYONECANPAY.value)
  }

  it must "default to SIGHASH_ALL if the given string/byte is not known" in {
    HashTypeOperations(Seq(0x124.toByte)) must be (SIGHASH_ALL(Int32(36)))
  }

  it must "find hashType for number 1190874345" in {
    //1190874345 & 0x80 = 0x80
    val num = Int32(1190874345)
    HashTypeOperations.fromNumber(num) must be (SIGHASH_ANYONECANPAY(num))
  }
}
