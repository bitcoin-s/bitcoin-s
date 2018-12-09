package org.bitcoins.core.script.crypto

import org.bitcoins.core.number.Int32
import org.scalatest.{FlatSpec, MustMatchers}
import scodec.bits.ByteVector

/**
  * Created by chris on 2/27/16.
  */
class HashTypeTest extends FlatSpec with MustMatchers {

  "HashType" must "combine hash types with SIGHASH_ANYONECANPAY" in {
    HashType.sigHashAllAnyoneCanPay.num must be(Int32(0x81))
    HashType.sigHashNoneAnyoneCanPay.num must be(Int32(0x82))
    HashType.sigHashSingleAnyoneCanPay.num must be(Int32(0x83))
  }

  it must "find a hash type by its hex value" in {
    HashType("00000001") must be(HashType.sigHashAll)
    HashType("00000002") must be(HashType.sigHashNone)
    HashType("00000003") must be(HashType.sigHashSingle)
    HashType("00000080") must be(HashType.sigHashAnyoneCanPay)
  }

  it must "find a hash type by its byte value" in {
    HashType(0.toByte) must be(SIGHASH_ALL(Int32.zero))
    HashType(1.toByte) must be(SIGHASH_ALL(Int32.one))
    HashType(2.toByte) must be(HashType.sigHashNone)
    HashType(3.toByte) must be(HashType.sigHashSingle)
    HashType(0x80) must be(HashType.sigHashAnyoneCanPay)

  }

  it must "default to SIGHASH_ALL if the given string/byte is not known" in {
    HashType(ByteVector(0x124.toByte)) must be(SIGHASH_ALL(Int32(36)))
  }

  it must "find hashType for number 1190874345" in {
    //1190874345 & 0x80 = 0x80
    val num = Int32(1190874345)
    HashType(num).isInstanceOf[SIGHASH_ANYONECANPAY] must be(true)
    HashType(num.bytes).isInstanceOf[SIGHASH_ANYONECANPAY] must be(true)
  }

  it must "determine if a given number is of hashType SIGHASH_ALL" in {
    HashType.isSigHashAll(Int32.zero) must be(true)
    HashType.isSigHashAll(Int32.one) must be(true)
    HashType.isSigHashAll(Int32(5)) must be(true)

    HashType.isSigHashAll(HashType.sigHashNone.num) must be(false)
    HashType.isSigHashAll(HashType.sigHashSingle.num) must be(false)
  }

  it must "return the correct byte for a given hashtype" in {
    SIGHASH_ALL(HashType.sigHashAllByte).byte must be(0x01.toByte)
    HashType.sigHashNone.byte must be(0x02.toByte)
    HashType.sigHashSingle.byte must be(0x03.toByte)
    HashType.sigHashAnyoneCanPay.byte must be(0x80.toByte)
    HashType.sigHashAllAnyoneCanPay.byte must be(0x81.toByte)
    HashType.sigHashNoneAnyoneCanPay.byte must be(0x82.toByte)
    HashType.sigHashSingleAnyoneCanPay.byte must be(0x83.toByte)
  }

  it must "intercept require statements for each hashType with illegal inputs" in {
    intercept[IllegalArgumentException] {
      SIGHASH_ALL(Int32(2))
    }
  }

  it must "find each specific hashType from byte sequence of default value" in {
    //tests each hashtypes overriding fromBytes function
    HashType(HashType.sigHashAll.num.bytes) must be(HashType.sigHashAll)
    HashType(HashType.sigHashNone.num.bytes) must be(HashType.sigHashNone)
    HashType(HashType.sigHashSingle.num.bytes) must be(HashType.sigHashSingle)
    HashType(HashType.sigHashAnyoneCanPay.num.bytes) must be(
      HashType.sigHashAnyoneCanPay)
    HashType(HashType.sigHashAllAnyoneCanPay.num.bytes) must be(
      HashType.sigHashAllAnyoneCanPay)
    HashType(HashType.sigHashNoneAnyoneCanPay.num.bytes) must be(
      HashType.sigHashNoneAnyoneCanPay)
    HashType(HashType.sigHashSingleAnyoneCanPay.num.bytes) must be(
      HashType.sigHashSingleAnyoneCanPay)
  }

  it must "find a hashtype with only an integer" in {
    HashType(105512910).isInstanceOf[SIGHASH_ANYONECANPAY] must be(true)
  }

}
