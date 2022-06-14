package org.bitcoins.crypto

import scodec.bits.ByteVector

/** Created by chris on 2/27/16.
  */
class HashTypeTest extends BitcoinSCryptoTest {

  "HashType" must "combine hash types with SIGHASH_ANYONECANPAY" in {
    HashType.sigHashAllAnyoneCanPay.num must be(0x81)
    HashType.sigHashNoneAnyoneCanPay.num must be(0x82)
    HashType.sigHashSingleAnyoneCanPay.num must be(0x83)
  }

  it must "find a hash type by its hex value" in {
    HashType("00000001") must be(HashType.sigHashAll)
    HashType("00000002") must be(HashType.sigHashNone)
    HashType("00000003") must be(HashType.sigHashSingle)
    HashType("00000080") must be(HashType.sigHashAnyoneCanPay)
  }

  it must "find a hash type by its byte value" in {
    HashType(0.toByte) must be(SIGHASH_ALL(0))
    HashType(1.toByte) must be(SIGHASH_ALL(1))
    HashType(2.toByte) must be(HashType.sigHashNone)
    HashType(3.toByte) must be(HashType.sigHashSingle)
    HashType(0x80) must be(HashType.sigHashAnyoneCanPay)

  }

  it must "default to SIGHASH_ALL if the given string/byte is not known" in {
    HashType(ByteVector(0x124.toByte)) must be(SIGHASH_ALL(36))
  }

  it must "find hashType for number 1190874345" in {
    //1190874345 & 0x80 = 0x80
    val num = 1190874345
    HashType(num).isInstanceOf[SIGHASH_ANYONECANPAY] must be(true)
    HashType(ByteVector.fromInt(num))
      .isInstanceOf[SIGHASH_ANYONECANPAY] must be(true)
  }

  it must "determine if a given number is of hashType SIGHASH_ALL" in {
    HashType.isSigHashAll(0) must be(true)
    HashType.isSigHashAll(1) must be(true)
    HashType.isSigHashAll(5) must be(true)

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
      SIGHASH_ALL(2)
    }
  }

  it must "find each specific hashType from byte sequence of default value" in {
    //tests each hashtypes overriding fromBytes function
    HashType(HashType.sigHashAll.num) must be(HashType.sigHashAll)
    HashType(HashType.sigHashNone.num) must be(HashType.sigHashNone)
    HashType(HashType.sigHashSingle.num) must be(HashType.sigHashSingle)
    HashType(HashType.sigHashAnyoneCanPay.num) must be(
      HashType.sigHashAnyoneCanPay)
    HashType(HashType.sigHashAllAnyoneCanPay.num) must be(
      HashType.sigHashAllAnyoneCanPay)
    HashType(HashType.sigHashNoneAnyoneCanPay.num) must be(
      HashType.sigHashNoneAnyoneCanPay)
    HashType(HashType.sigHashSingleAnyoneCanPay.num) must be(
      HashType.sigHashSingleAnyoneCanPay)
  }

  it must "find a hashtype with only an integer" in {
    HashType(105512910).isInstanceOf[SIGHASH_ANYONECANPAY] must be(true)
  }

  it must "have serialization symmetry" in {
    forAll(NumberGenerator.ints.map(ByteVector.fromInt(_))) { i32 =>
      val hashType = HashType.fromBytes(i32)

      hashType.num == i32.toInt() &&
      i32.last == hashType.byte &&
      //this check cannot check the other 3 bytes in
      //hash type as they are discarded from inclusion
      //on a bitcoin digital signature. Not sure why satoshi
      //would have just used a uint8_t to represent a hash type
      //instead of a uint32_t.
      HashType.fromByte(hashType.byte).byte == hashType.byte
    }

    forAll(CryptoGenerators.hashType) { hashType =>
      assert(HashType.fromByte(hashType.byte) == hashType)
    }
  }
}
