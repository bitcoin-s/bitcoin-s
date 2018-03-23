package org.bitcoins.core.script.crypto

import org.bitcoins.core.crypto.ECDigitalSignature
import org.bitcoins.core.number.Int32
import org.bitcoins.core.util.Factory

/**
 * Created by chris on 1/18/16.
 */
sealed trait HashType {
  def num: Int32
  def byte: Byte
}

object HashType extends Factory[HashType] {
  def fromBytes(bytes: Seq[Byte]): HashType = {
    val num = Int32(bytes)
    fromNumber(num)
  }

  def fromByte(byte: Byte): HashType = fromBytes(Seq(byte))

  def fromNumber(num: Int32): HashType = {
    if (isSIGHASH_NONE(num)) {
      if (isSIGHASH_NONE_ANYONECANPAY(num)) SIGHASH_NONE_ANYONECANPAY(num) else SIGHASH_NONE(num)
    } else if (isSIGHASH_SINGLE(num)) {
      if (isSIGHASH_SINGLE_ANYONECANPAY(num)) SIGHASH_SINGLE_ANYONECANPAY(num) else SIGHASH_SINGLE(num)
    } else if (isSIGHASH_ANYONECANPAY(num)) {
      if (isSIGHASH_ALL_ANYONECANPAY(num)) SIGHASH_ALL_ANYONECANPAY(num)
      else {
        require(isONLY_ANYONE_CANPAY(num))
        SIGHASH_ANYONECANPAY(num)
      }
    } else SIGHASH_ALL(num)
  }

  /** Returns a hashtype's default byte value */
  def byte(hashType: HashType): Byte = hashType match {
    case _: SIGHASH_ALL => sigHashAllByte
    case h: HashType    => h.byte
  }

  def isSIGHASH_ALL_ONE(num: Int32): Boolean = (num & Int32(0x1f)) == Int32(1)
  def isSIGHASH_NONE(num: Int32): Boolean = (num & Int32(0x1f)) == Int32(2)
  def isSIGHASH_SINGLE(num: Int32): Boolean = (num & Int32(0x1f)) == Int32(3)
  def isSIGHASH_ANYONECANPAY(num: Int32): Boolean = (num & Int32(0x80)) == Int32(0x80)
  def isSIGHASH_ALL_ANYONECANPAY(num: Int32): Boolean = isSIGHASH_ALL_ONE(num) && isSIGHASH_ANYONECANPAY(num)
  def isSIGHASH_NONE_ANYONECANPAY(num: Int32): Boolean = isSIGHASH_NONE(num) && isSIGHASH_ANYONECANPAY(num)
  def isSIGHASH_SINGLE_ANYONECANPAY(num: Int32): Boolean = isSIGHASH_SINGLE(num) && isSIGHASH_ANYONECANPAY(num)
  def isSIGHASH_ALL(num: Int32): Boolean = {
    if (!(isSIGHASH_NONE(num) || isSIGHASH_SINGLE(num) || isSIGHASH_ANYONECANPAY(num) || isSIGHASH_ALL_ANYONECANPAY(num) ||
      isSIGHASH_SINGLE_ANYONECANPAY(num) || isSIGHASH_NONE_ANYONECANPAY(num))) true
    else false
  }
  def isONLY_ANYONE_CANPAY(num: Int32): Boolean = {
    !(HashType.isSIGHASH_ALL_ANYONECANPAY(num) || HashType.isSIGHASH_NONE_ANYONECANPAY(num) ||
      HashType.isSIGHASH_SINGLE_ANYONECANPAY(num))
  }

  /** Checks if the given hash type has the ANYONECANPAY bit set */
  def isAnyoneCanPay(hashType: HashType): Boolean = hashType match {
    case _: SIGHASH_ANYONECANPAY | _: SIGHASH_ALL_ANYONECANPAY | _: SIGHASH_SINGLE_ANYONECANPAY |
      _: SIGHASH_NONE_ANYONECANPAY => true
    case _: SIGHASH_ALL | _: SIGHASH_SINGLE | _: SIGHASH_NONE => false
  }

  lazy val hashTypes = Seq(sigHashAll, sigHashNone, sigHashSingle, sigHashAnyoneCanPay,
    sigHashNoneAnyoneCanPay, sigHashAllAnyoneCanPay, sigHashSingleAnyoneCanPay)

  lazy val hashTypeBytes: Seq[Byte] = Seq(sigHashAllByte, sigHashSingleByte, sigHashNoneByte, sigHashAnyoneCanPayByte,
    sigHashNoneAnyoneCanPayByte, sigHashSingleAnyoneCanPayByte, sigHashAllAnyoneCanPayByte)
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

  val sigHashAnyoneCanPay: SIGHASH_ANYONECANPAY = SIGHASH_ANYONECANPAY(sigHashAnyoneCanPayNum)

  /** The default byte for [[SIGHASH_NONE]] */
  val sigHashNoneByte: Byte = 2.toByte

  val sigHashNone: SIGHASH_NONE = SIGHASH_NONE(Int32(sigHashNoneByte))

  /** The default byte for [[SIGHASH_SINGLE]] */
  val sigHashSingleByte: Byte = 3.toByte

  val sigHashSingle: SIGHASH_SINGLE = SIGHASH_SINGLE(Int32(sigHashSingleByte))

  val sigHashAllAnyoneCanPayByte = (HashType.sigHashAllByte | HashType.sigHashAnyoneCanPayByte).toByte

  val sigHashAllAnyoneCanPayNum = (Int32(sigHashAllByte) | sigHashAnyoneCanPayNum)

  val sigHashAllAnyoneCanPay = SIGHASH_ALL_ANYONECANPAY(sigHashAllAnyoneCanPayNum)

  val sigHashNoneAnyoneCanPayByte = (HashType.sigHashNoneByte | HashType.sigHashAnyoneCanPayByte).toByte

  val sigHashNoneAnyoneCanPayNum = (Int32(sigHashNoneByte) | sigHashAnyoneCanPayNum)

  val sigHashNoneAnyoneCanPay = SIGHASH_NONE_ANYONECANPAY(sigHashNoneAnyoneCanPayNum)

  val sigHashSingleAnyoneCanPayByte = (HashType.sigHashSingleByte | HashType.sigHashAnyoneCanPayByte).toByte

  val sigHashSingleAnyoneCanPayNum = (Int32(sigHashSingleByte) | sigHashAnyoneCanPayNum)

  val sigHashSingleAnyoneCanPay = SIGHASH_SINGLE_ANYONECANPAY(sigHashSingleAnyoneCanPayNum)

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
  require(HashType.isSIGHASH_ALL(num), "SIGHASH_ALL acts as a 'catch-all' for undefined hashtypes, and has a default " +
    "value of one. Your input was: " + num + ", which is of hashType: " + HashType(num))
  override def byte = HashType.sigHashAllByte
}
object SIGHASH_ALL {
  def apply(byte: Byte): SIGHASH_ALL = SIGHASH_ALL(Int32(byte))
}

case class SIGHASH_NONE(override val num: Int32) extends HashType {
  require(HashType.isSIGHASH_NONE(num), "The given number is not a SIGHASH_NONE number: " + num)
  override def byte: Byte = HashType.sigHashNoneByte
}

case class SIGHASH_SINGLE(override val num: Int32) extends HashType {
  require(HashType.isSIGHASH_SINGLE(num), "The given number is not a SIGHASH_SINGLE number: " + num)
  override def byte: Byte = HashType.sigHashSingleByte
}

case class SIGHASH_ANYONECANPAY(override val num: Int32) extends HashType {
  require(HashType.isSIGHASH_ANYONECANPAY(num), "The given number was not a SIGHASH_ANYONECANPAY number: " + num)
  override def byte: Byte = HashType.sigHashAnyoneCanPayByte
}

case class SIGHASH_ALL_ANYONECANPAY(override val num: Int32) extends HashType {
  require(HashType.isSIGHASH_ALL_ANYONECANPAY(num), "The given number was not a SIGHASH_ALL_ANYONECANPAY number: " + num)
  override def byte: Byte = HashType.sigHashAllAnyoneCanPayByte
}

case class SIGHASH_NONE_ANYONECANPAY(override val num: Int32) extends HashType {
  require(HashType.isSIGHASH_NONE_ANYONECANPAY(num), "The given number was not a SIGHASH_NONE_ANYONECANPAY number: " + num)
  override def byte: Byte = HashType.sigHashNoneAnyoneCanPayByte
}

case class SIGHASH_SINGLE_ANYONECANPAY(override val num: Int32) extends HashType {
  require(HashType.isSIGHASH_SINGLE_ANYONECANPAY(num), "The given number was not a SIGHASH_SINGLE_ANYONECANPAY number: " + num)
  override def byte: Byte = HashType.sigHashSingleAnyoneCanPayByte
}
