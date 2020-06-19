package org.bitcoins.core.script.crypto

import org.bitcoins.core.number.Int32
import org.bitcoins.crypto.{ECDigitalSignature, Factory}
import scodec.bits.ByteVector

/**
  * Created by chris on 1/18/16.
  */
sealed trait HashType {
  def num: Int32
  def byte: Byte = num.bytes.last
}

object HashType extends Factory[HashType] {

  def fromBytes(bytes: ByteVector): HashType = {
    val num = Int32(bytes)
    fromNumber(num)
  }

  def fromByte(byte: Byte): HashType = fromBytes(ByteVector.fromByte(byte))

  def fromNumber(num: Int32): HashType = {
    if (isSigHashNone(num)) {
      if (isSigHashNoneAnyoneCanPay(num)) {
        SIGHASH_NONE_ANYONECANPAY(num)
      } else {
        SIGHASH_NONE(num)
      }
    } else if (isSigHashSingle(num)) {
      if (isSigHashAnyoneCanPay(num)) {
        SIGHASH_SINGLE_ANYONECANPAY(num)
      } else {
        SIGHASH_SINGLE(num)
      }
    } else if (isSigHashAnyoneCanPay(num)) {
      if (isSigHashAllAnyoneCanPay(num)) {
        SIGHASH_ALL_ANYONECANPAY(num)
      } else {
        require(isOnlyAnyoneCanPay(num))
        SIGHASH_ANYONECANPAY(num)
      }
    } else SIGHASH_ALL(num)
  }

  /** Returns a hashtype's default byte value */
  def byte(hashType: HashType): Byte =
    hashType match {
      case _: SIGHASH_ALL => sigHashAllByte
      case h: HashType    => h.byte
    }

  def isSigHashAllOne(num: Int32): Boolean = (num & Int32(0x1f)) == Int32(1)

  def isSigHashNone(num: Int32): Boolean = (num & Int32(0x1f)) == Int32(2)

  def isSigHashSingle(num: Int32): Boolean = (num & Int32(0x1f)) == Int32(3)

  def isSigHashAnyoneCanPay(num: Int32): Boolean =
    (num & Int32(0x80)) == Int32(0x80)

  def isSigHashAllAnyoneCanPay(num: Int32): Boolean = {
    isSigHashAllOne(num) && isSigHashAnyoneCanPay(num)
  }

  def isSigHashNoneAnyoneCanPay(num: Int32): Boolean = {
    isSigHashNone(num) && isSigHashAnyoneCanPay(num)
  }

  def isSigHashSingleAnyoneCanPay(num: Int32): Boolean = {
    isSigHashSingle(num) && isSigHashAnyoneCanPay(num)
  }

  def isSigHashAll(num: Int32): Boolean = {
    if (
      !(isSigHashNone(num) ||
        isSigHashSingle(num) ||
        isSigHashAnyoneCanPay(num) ||
        isSigHashAllAnyoneCanPay(num) ||
        isSigHashSingleAnyoneCanPay(num) ||
        isSigHashNoneAnyoneCanPay(num))
    ) true
    else false
  }

  def isOnlyAnyoneCanPay(num: Int32): Boolean = {
    !(HashType.isSigHashAllAnyoneCanPay(num) ||
      HashType.isSigHashNoneAnyoneCanPay(num) ||
      HashType.isSigHashSingleAnyoneCanPay(num))
  }

  /** Checks if the given hash type has the ANYONECANPAY bit set */
  def isAnyoneCanPay(hashType: HashType): Boolean =
    hashType match {
      case _: SIGHASH_ANYONECANPAY | _: SIGHASH_ALL_ANYONECANPAY |
          _: SIGHASH_SINGLE_ANYONECANPAY | _: SIGHASH_NONE_ANYONECANPAY =>
        true
      case _: SIGHASH_ALL | _: SIGHASH_SINGLE | _: SIGHASH_NONE => false
    }

  lazy val hashTypes = Seq(sigHashAll,
                           sigHashNone,
                           sigHashSingle,
                           sigHashAnyoneCanPay,
                           sigHashNoneAnyoneCanPay,
                           sigHashAllAnyoneCanPay,
                           sigHashSingleAnyoneCanPay)

  lazy val hashTypeBytes: Vector[Byte] = Vector(
    sigHashAllByte,
    sigHashSingleByte,
    sigHashNoneByte,
    sigHashAnyoneCanPayByte,
    sigHashNoneAnyoneCanPayByte,
    sigHashSingleAnyoneCanPayByte,
    sigHashAllAnyoneCanPayByte
  )

  def apply(num: Int32): HashType = fromNumber(num)

  def apply(int: Int): HashType = HashType(Int32(int))

  def apply(byte: Byte): HashType = fromByte(byte)

  /** The default byte used to represent [[SIGHASH_ALL]] */
  val sigHashAllByte = 1.toByte

  /** The default [[SIGHASH_ALL]] value */
  val sigHashAll = SIGHASH_ALL(sigHashAllByte)

  /**
    * The default num for [[SIGHASH_ANYONECANPAY]]
    * We need this for serialization of [[HashType]]
    * flags inside of [[org.bitcoins.core.crypto.TransactionSignatureSerializer]]
    *
    * Have to be careful using this value, since native scala numbers are signed
    * We need this because this serializes to 0x00000080 instead of 0xffffff80
    * If we try to use Int32(sigHashAnyoneCanPayByte) we will get the latter serialization
    * because all native scala numbers are signed
    */
  val sigHashAnyoneCanPayNum = Int32(0x80)

  val sigHashAnyoneCanPayByte = 0x80.toByte

  val sigHashAnyoneCanPay: SIGHASH_ANYONECANPAY = SIGHASH_ANYONECANPAY(
    sigHashAnyoneCanPayNum)

  /** The default byte for [[SIGHASH_NONE]] */
  val sigHashNoneByte: Byte = 2.toByte

  val sigHashNone: SIGHASH_NONE = SIGHASH_NONE(Int32(sigHashNoneByte))

  /** The default byte for [[SIGHASH_SINGLE]] */
  val sigHashSingleByte: Byte = 3.toByte

  val sigHashSingle: SIGHASH_SINGLE = SIGHASH_SINGLE(Int32(sigHashSingleByte))

  val sigHashAllAnyoneCanPayByte =
    (HashType.sigHashAllByte | HashType.sigHashAnyoneCanPayByte).toByte

  val sigHashAllAnyoneCanPayNum = Int32(sigHashAllByte) | sigHashAnyoneCanPayNum

  val sigHashAllAnyoneCanPay = SIGHASH_ALL_ANYONECANPAY(
    sigHashAllAnyoneCanPayNum)

  val sigHashNoneAnyoneCanPayByte =
    (HashType.sigHashNoneByte | HashType.sigHashAnyoneCanPayByte).toByte

  val sigHashNoneAnyoneCanPayNum =
    Int32(sigHashNoneByte) | sigHashAnyoneCanPayNum

  val sigHashNoneAnyoneCanPay = SIGHASH_NONE_ANYONECANPAY(
    sigHashNoneAnyoneCanPayNum)

  val sigHashSingleAnyoneCanPayByte =
    (HashType.sigHashSingleByte | HashType.sigHashAnyoneCanPayByte).toByte

  val sigHashSingleAnyoneCanPayNum =
    Int32(sigHashSingleByte) | sigHashAnyoneCanPayNum

  val sigHashSingleAnyoneCanPay = SIGHASH_SINGLE_ANYONECANPAY(
    sigHashSingleAnyoneCanPayNum)

  /**
    * Checks if the given digital signature has a valid hash type
    * Mimics this functionality inside of Bitcoin Core
    * https://github.com/bitcoin/bitcoin/blob/b83264d9c7a8ddb79f64bd9540caddc8632ef31f/src/script/interpreter.cpp#L186
    */
  def isDefinedHashtypeSignature(sig: ECDigitalSignature): Boolean = {
    sig.bytes.nonEmpty && hashTypeBytes.contains(sig.bytes.last)
  }
}

/**
  * defaultValue is the underlying value of the HashType. The last byte of a signature determines the HashType.
  * https://en.bitcoin.it/wiki/OP_CHECKSIG
  */
case class SIGHASH_ALL(override val num: Int32) extends HashType {
  require(
    HashType.isSigHashAll(num),
    "SIGHASH_ALL acts as a 'catch-all' for undefined hashtypes, and has a default " +
      "value of one. Your input was: " + num + ", which is of hashType: " + HashType(
      num)
  )
}

object SIGHASH_ALL {
  def apply(byte: Byte): SIGHASH_ALL = SIGHASH_ALL(Int32(byte))
}

case class SIGHASH_NONE(override val num: Int32) extends HashType {
  require(HashType.isSigHashNone(num),
          "The given number is not a SIGHASH_NONE number: " + num)
}

case class SIGHASH_SINGLE(override val num: Int32) extends HashType {
  require(HashType.isSigHashSingle(num),
          "The given number is not a SIGHASH_SINGLE number: " + num)
}

case class SIGHASH_ANYONECANPAY(override val num: Int32) extends HashType {
  require(HashType.isSigHashAnyoneCanPay(num),
          "The given number was not a SIGHASH_ANYONECANPAY number: " + num)
}

case class SIGHASH_ALL_ANYONECANPAY(override val num: Int32) extends HashType {
  require(HashType.isSigHashAllAnyoneCanPay(num),
          "The given number was not a SIGHASH_ALL_ANYONECANPAY number: " + num)
}

case class SIGHASH_NONE_ANYONECANPAY(override val num: Int32) extends HashType {
  require(HashType.isSigHashNoneAnyoneCanPay(num),
          "The given number was not a SIGHASH_NONE_ANYONECANPAY number: " + num)
}

case class SIGHASH_SINGLE_ANYONECANPAY(override val num: Int32)
    extends HashType {
  require(
    HashType.isSigHashSingleAnyoneCanPay(num),
    "The given number was not a SIGHASH_SINGLE_ANYONECANPAY number: " + num)
}
