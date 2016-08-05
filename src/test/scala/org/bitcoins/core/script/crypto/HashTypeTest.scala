package org.bitcoins.core.script.crypto

import org.bitcoins.core.number.Int32
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/27/16.
 */
class HashTypeTest extends FlatSpec with MustMatchers {

  "HashType" must "combine hash types with SIGHASH_ANYONECANPAY" in {
    SIGHASH_ALL_ANYONECANPAY.defaultValue.num must be (Int32(0x81))
    SIGHASH_NONE_ANYONECANPAY.defaultValue.num must be (Int32(0x82))
    SIGHASH_SINGLE_ANYONECANPAY.defaultValue.num must be (Int32(0x83))
  }

  it must "find a hash type by its hex value" in {
    HashType("00000001") must be (SIGHASH_ALL.defaultValue)
    HashType("00000002") must be (SIGHASH_NONE.defaultValue)
    HashType("00000003") must be (SIGHASH_SINGLE.defaultValue)
    HashType("00000080") must be (SIGHASH_ANYONECANPAY.defaultValue)
  }

  it must "find a hash type by its byte value" in {

    HashType(Seq(0.toByte)) must be (SIGHASH_ALL(Int32.zero))
    HashType(Seq(1.toByte)) must be (SIGHASH_ALL(Int32.one))
    HashType(Seq(2.toByte)) must be (SIGHASH_NONE.defaultValue)
    HashType(Seq(3.toByte)) must be (SIGHASH_SINGLE.defaultValue)
    HashType(Int32(0x80).bytes) must be (SIGHASH_ANYONECANPAY.defaultValue)
  }

  it must "default to SIGHASH_ALL if the given string/byte is not known" in {
    HashType(Seq(0x124.toByte)) must be (SIGHASH_ALL(Int32(36)))
  }

  it must "find hashType for number 1190874345" in {
    //1190874345 & 0x80 = 0x80
    val num = Int32(1190874345)
    HashType.fromNumber(num) must be (SIGHASH_ANYONECANPAY(num))
  }

  it must "determine if a given number is of hashType SIGHASH_ALL" in {
    HashType.isSIGHASH_ALL(Int32.zero) must be (true)
    HashType.isSIGHASH_ALL(Int32.one) must be (true)
    HashType.isSIGHASH_ALL(Int32(5)) must be (true)

    HashType.isSIGHASH_ALL(SIGHASH_NONE.defaultValue.num) must be (false)
    HashType.isSIGHASH_ALL(SIGHASH_SINGLE.defaultValue.num) must be (false)
  }
}
