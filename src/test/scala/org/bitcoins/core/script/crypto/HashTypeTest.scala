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
    HashType(num) must be (SIGHASH_ANYONECANPAY(num))
  }

  it must "determine if a given number is of hashType SIGHASH_ALL" in {
    HashType.isSIGHASH_ALL(Int32.zero) must be (true)
    HashType.isSIGHASH_ALL(Int32.one) must be (true)
    HashType.isSIGHASH_ALL(Int32(5)) must be (true)

    HashType.isSIGHASH_ALL(SIGHASH_NONE.defaultValue.num) must be (false)
    HashType.isSIGHASH_ALL(SIGHASH_SINGLE.defaultValue.num) must be (false)
  }

  it must "return the correct byte for a given hashtype" in {
    HashType.byte(SIGHASH_ALL.defaultValue) must be (0x01.toByte)
    HashType.byte(SIGHASH_NONE.defaultValue) must be (0x02.toByte)
    HashType.byte(SIGHASH_SINGLE.defaultValue) must be (0x03.toByte)
    HashType.byte(SIGHASH_ANYONECANPAY.defaultValue) must be (0x80.toByte)
    HashType.byte(SIGHASH_ALL_ANYONECANPAY.defaultValue) must be (0x81.toByte)
    HashType.byte(SIGHASH_NONE_ANYONECANPAY.defaultValue) must be (0x82.toByte)
    HashType.byte(SIGHASH_SINGLE_ANYONECANPAY.defaultValue) must be (0x83.toByte)
  }

  it must "intercept require statements for each hashType with illegal inputs" in {
    intercept[IllegalArgumentException]{
      SIGHASH_ALL(Int32(2))
    }
    intercept[IllegalArgumentException]{
      SIGHASH_NONE(Int32(5))
    }
    intercept[IllegalArgumentException]{
      SIGHASH_SINGLE(Int32(10))
    }
    intercept[IllegalArgumentException]{
      SIGHASH_ANYONECANPAY(Int32(50))
    }
    intercept[IllegalArgumentException]{
      SIGHASH_ALL_ANYONECANPAY(Int32(0x80))
    }
    intercept[IllegalArgumentException]{
      SIGHASH_NONE_ANYONECANPAY(Int32(0x80))
    }
    intercept[IllegalArgumentException]{
      SIGHASH_SINGLE_ANYONECANPAY(Int32(0x80))
    }
  }

  it must "find each specific hashType from byte sequence of default value" in {
    SIGHASH_ALL(SIGHASH_ALL.defaultValue.num.bytes).isInstanceOf[SIGHASH_ALL] must be (true)
    SIGHASH_NONE(SIGHASH_NONE.defaultValue.num.bytes).isInstanceOf[SIGHASH_NONE] must be (true)
    SIGHASH_SINGLE(SIGHASH_SINGLE.defaultValue.num.bytes).isInstanceOf[SIGHASH_SINGLE] must be (true)
    SIGHASH_ANYONECANPAY(SIGHASH_ANYONECANPAY.defaultValue.num.bytes).isInstanceOf[SIGHASH_ANYONECANPAY] must be (true)
    SIGHASH_ALL_ANYONECANPAY(SIGHASH_ALL_ANYONECANPAY.defaultValue.num.bytes).isInstanceOf[SIGHASH_ALL_ANYONECANPAY] must be (true)
    SIGHASH_NONE_ANYONECANPAY(SIGHASH_NONE_ANYONECANPAY.defaultValue.num.bytes).isInstanceOf[SIGHASH_NONE_ANYONECANPAY] must be (true)
    SIGHASH_SINGLE_ANYONECANPAY(SIGHASH_SINGLE_ANYONECANPAY.defaultValue.num.bytes).isInstanceOf[SIGHASH_SINGLE_ANYONECANPAY] must be (true)
  }

  it must "verify default byte value of each hashtype" in {
    SIGHASH_ALL.byte must be (0x01.toByte)
    SIGHASH_NONE.byte must be (0x02.toByte)
    SIGHASH_SINGLE.byte must be (0x03.toByte)
    SIGHASH_ANYONECANPAY.byte must be (0x80.toByte)
    SIGHASH_ALL_ANYONECANPAY.byte must be (0x81.toByte)
    SIGHASH_NONE_ANYONECANPAY.byte must be (0x82.toByte)
    SIGHASH_SINGLE_ANYONECANPAY.byte must be (0x83.toByte)
  }

  it must "find a hashtype with only an integer" in {
    HashType(105512910).isInstanceOf[SIGHASH_ANYONECANPAY] must be (true)
  }

}
