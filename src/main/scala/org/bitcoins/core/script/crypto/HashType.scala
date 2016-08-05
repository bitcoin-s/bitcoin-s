package org.bitcoins.core.script.crypto

import org.bitcoins.core.number.Int32
import org.bitcoins.core.util.Factory

/**
 * Created by chris on 1/18/16.
 */
sealed trait HashType {
  def num : Int32
  def byte : Byte
}

object HashType extends Factory[HashType] {
  def fromBytes(bytes : Seq[Byte]) : HashType = {
    val num = Int32(bytes)
    fromNumber(num)
  }

  def fromNumber(num : Int32) : HashType = {
    if (isSIGHASH_NONE(num)) {
      if (isSIGHASH_NONE_ANYONECANPAY(num)) SIGHASH_NONE_ANYONECANPAY(num) else SIGHASH_NONE(num)
    }
    else if (isSIGHASH_SINGLE(num)) {
      if (isSIGHASH_SINGLE_ANYONECANPAY(num)) SIGHASH_SINGLE_ANYONECANPAY(num) else SIGHASH_SINGLE(num)
    }
    else if (isSIGHASH_ANYONECANPAY(num)) {
      if (isSIGHASH_ALL_ANYONECANPAY(num)) SIGHASH_ALL_ANYONECANPAY(num)
      else {
        require(isONLY_ANYONE_CANPAY(num))
        SIGHASH_ANYONECANPAY(num)
      }
    }
    else {
      SIGHASH_ALL(num)
    }
  }

  def byte (hashType : HashType) : Byte = hashType match {
    case _ : SIGHASH_ALL => SIGHASH_ALL.defaultValue.num.underlying.toByte
    case _ : SIGHASH_NONE => SIGHASH_NONE.defaultValue.num.underlying.toByte
    case _ : SIGHASH_SINGLE => SIGHASH_SINGLE.defaultValue.num.underlying.toByte
    case _ : SIGHASH_ANYONECANPAY => SIGHASH_ANYONECANPAY.defaultValue.num.underlying.toByte
    case _ : SIGHASH_ALL_ANYONECANPAY => SIGHASH_ALL_ANYONECANPAY.defaultValue.num.underlying.toByte
    case _ : SIGHASH_NONE_ANYONECANPAY => SIGHASH_NONE_ANYONECANPAY.defaultValue.num.underlying.toByte
    case _ : SIGHASH_SINGLE_ANYONECANPAY => SIGHASH_SINGLE_ANYONECANPAY.defaultValue.num.underlying.toByte
  }

  def isSIGHASH_ALL_ONE(num : Int32) : Boolean = (num & Int32(0x1f)) == Int32(1)
  def isSIGHASH_NONE(num : Int32) : Boolean = (num & Int32(0x1f)) == Int32(2)
  def isSIGHASH_SINGLE(num : Int32) : Boolean = (num & Int32(0x1f)) == Int32(3)
  def isSIGHASH_ANYONECANPAY(num : Int32) : Boolean = (num & Int32(0x80)) == Int32(0x80)
  def isSIGHASH_ALL_ANYONECANPAY(num : Int32) : Boolean = isSIGHASH_ALL_ONE(num) && isSIGHASH_ANYONECANPAY(num)
  def isSIGHASH_NONE_ANYONECANPAY(num : Int32) : Boolean = isSIGHASH_NONE(num) && isSIGHASH_ANYONECANPAY(num)
  def isSIGHASH_SINGLE_ANYONECANPAY(num: Int32) : Boolean = isSIGHASH_SINGLE(num) && isSIGHASH_ANYONECANPAY(num)
  def isSIGHASH_ALL(num : Int32) : Boolean = {
    if (!(isSIGHASH_NONE(num) || isSIGHASH_SINGLE(num) || isSIGHASH_ANYONECANPAY(num) || isSIGHASH_ALL_ANYONECANPAY(num) ||
      isSIGHASH_SINGLE_ANYONECANPAY(num) || isSIGHASH_NONE_ANYONECANPAY(num))) true
    else false
  }
  def isONLY_ANYONE_CANPAY(num : Int32) : Boolean = {
    !(HashType.isSIGHASH_ALL_ANYONECANPAY(num) || HashType.isSIGHASH_NONE_ANYONECANPAY(num) || HashType.isSIGHASH_SINGLE_ANYONECANPAY(num))
  }

  val hashTypes = Seq(SIGHASH_ALL.defaultValue, SIGHASH_NONE, SIGHASH_SINGLE, SIGHASH_ANYONECANPAY,
    SIGHASH_NONE_ANYONECANPAY, SIGHASH_ALL_ANYONECANPAY, SIGHASH_SINGLE_ANYONECANPAY)

  def apply(num : Int32) : HashType = fromNumber(num)
  def apply(int : Int) : HashType = {
    val num : Int32 = Int32(int)
    fromNumber(num)
  }
}

/**
  * defaultValue is the underlying value of the HashType. The last byte of a signature determines the HashType.
  * https://en.bitcoin.it/wiki/OP_CHECKSIG
  */

 object SIGHASH_ALL extends Factory[SIGHASH_ALL] {
  private case class SIGHASH_ALLImpl(num: Int32, byte : Byte = 0x01.toByte) extends SIGHASH_ALL {
    require(HashType.isSIGHASH_ALL(num), "SIGHASH_ALL acts as a 'catch-all' for undefined hashtypes, and has a default " +
      "value of one. Your input was: " + num + ", which is of hashType: " + HashType(num))
  }
  def byte : Byte = HashType.byte(defaultValue)
  def defaultValue : SIGHASH_ALL = SIGHASH_ALL(Int32.one)
  def apply(num : Int32) : SIGHASH_ALL = SIGHASH_ALLImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : SIGHASH_ALL = SIGHASH_ALL(Int32(bytes))
}

 object SIGHASH_NONE extends Factory[SIGHASH_NONE] {
  private case class SIGHASH_NONEImpl(num : Int32, byte : Byte = 0x02.toByte) extends SIGHASH_NONE {
    require(HashType.isSIGHASH_NONE(num), "The bitwise AND of 'num & 0x1f' must be 2 for a hashtype of SIGHASH_NONE. " +
      "Your input was: " + num + ", which is of hashType: " + HashType(num))
  }
  def byte : Byte = HashType.byte(defaultValue)
  def defaultValue : SIGHASH_NONE = SIGHASH_NONE(Int32(2))
  def apply(num : Int32) : SIGHASH_NONE = SIGHASH_NONEImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : SIGHASH_NONE = SIGHASH_NONE(Int32(bytes))
}

object SIGHASH_SINGLE extends Factory[SIGHASH_SINGLE] {
  private case class SIGHASH_SINGLEImpl(num : Int32, byte : Byte = 0x03.toByte) extends SIGHASH_SINGLE {
    require(HashType.isSIGHASH_SINGLE(num), "The bitwise AND of 'num & 0x1f' must be 3 for a hashtype of SIGHASH_SINGLE." +
      " Your input was: " + num + ", which is of hashType: " + HashType(num))
  }
  def byte : Byte = HashType.byte(defaultValue)
  def defaultValue : SIGHASH_SINGLE = SIGHASH_SINGLE(Int32(3))
  def apply(num : Int32) : SIGHASH_SINGLE = SIGHASH_SINGLEImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : SIGHASH_SINGLE = SIGHASH_SINGLE(Int32(bytes))
}

object SIGHASH_ANYONECANPAY extends Factory[SIGHASH_ANYONECANPAY] {
  private case class SIGHASH_ANYONECANPAYImpl(num : Int32, byte : Byte = 0x80.toByte) extends SIGHASH_ANYONECANPAY {
    require(HashType.isSIGHASH_ANYONECANPAY(num) && HashType.isONLY_ANYONE_CANPAY(num), "The bitwise AND of 'num & 0x80' must be 0x80 (or 128) for a hashtype of " +
      "SIGHASH_ANYONECANPAY. Your input was: " + num + ", which is of hashType: " + HashType(num))
  }
  def byte : Byte = HashType.byte(defaultValue)
  def defaultValue : SIGHASH_ANYONECANPAY = SIGHASH_ANYONECANPAY(Int32(0x80))
  def apply(num : Int32) : SIGHASH_ANYONECANPAY = SIGHASH_ANYONECANPAYImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : SIGHASH_ANYONECANPAY = SIGHASH_ANYONECANPAY(Int32(bytes))
}

object SIGHASH_ALL_ANYONECANPAY extends Factory[SIGHASH_ALL_ANYONECANPAY] {
  private case class SIGHASH_ALL_ANYONECANPAYImpl(num : Int32, byte : Byte = 0x81.toByte) extends SIGHASH_ALL_ANYONECANPAY {
    require(HashType.isSIGHASH_ALL_ANYONECANPAY(num), "SIGHASH_ALL_ANYONECANPAY must be of both hashTypes: SIGHASH_ALL, and " +
      "SIGHASH_ANYONECANPAY. Your input was: " + num + ", which is of hashType: " + HashType(num))
  }
  def byte : Byte = HashType.byte(defaultValue)
  def defaultValue : SIGHASH_ALL_ANYONECANPAY = SIGHASH_ALL_ANYONECANPAY(SIGHASH_ANYONECANPAY.defaultValue.num | SIGHASH_ALL.defaultValue.num)
  def apply(num : Int32) : SIGHASH_ALL_ANYONECANPAY = SIGHASH_ALL_ANYONECANPAYImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : SIGHASH_ALL_ANYONECANPAY = SIGHASH_ALL_ANYONECANPAY(Int32(bytes))
}

object SIGHASH_NONE_ANYONECANPAY extends Factory[SIGHASH_NONE_ANYONECANPAY] {
  private case class SIGHASH_NONE_ANYONECANPAYImpl(num : Int32, byte : Byte = 0x82.toByte) extends SIGHASH_NONE_ANYONECANPAY {
    require(HashType.isSIGHASH_NONE_ANYONECANPAY(num), "SIGHASH_NONE_ANYONECANPAY must be of both hashTypes: SIGHASH_NONE, and " +
      "SIGHASH_ANYONECANPAY. Your input was: " + num + ", which is of hashType: " + HashType(num))
  }
  def byte : Byte = HashType.byte(defaultValue)
  def defaultValue : SIGHASH_NONE_ANYONECANPAY = SIGHASH_NONE_ANYONECANPAY(SIGHASH_ANYONECANPAY.defaultValue.num | SIGHASH_NONE.defaultValue.num)
  def apply(num : Int32) : SIGHASH_NONE_ANYONECANPAY = SIGHASH_NONE_ANYONECANPAYImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : SIGHASH_NONE_ANYONECANPAY = SIGHASH_NONE_ANYONECANPAY(Int32(bytes))
}

object SIGHASH_SINGLE_ANYONECANPAY extends Factory[SIGHASH_SINGLE_ANYONECANPAY] {
  private case class SIGHASH_SINGLE_ANYONECANPAYImpl(num : Int32, byte : Byte = 0x83.toByte) extends SIGHASH_SINGLE_ANYONECANPAY {
    require(HashType.isSIGHASH_SINGLE_ANYONECANPAY(num), "SIGHASH_SINGLE_ANYONECANPAY must be of both hashTypes: SIGHASH_SINGLE, " +
      "and SIGHASH_ANYONECANPAY. Your input was: " + num + ", which is of hashType: " + HashType(num))
  }
  def byte : Byte = HashType.byte(defaultValue)
  def defaultValue : SIGHASH_SINGLE_ANYONECANPAY = SIGHASH_SINGLE_ANYONECANPAY(SIGHASH_ANYONECANPAY.defaultValue.num | SIGHASH_SINGLE.defaultValue.num)
  def apply(num : Int32) : SIGHASH_SINGLE_ANYONECANPAY = SIGHASH_SINGLE_ANYONECANPAYImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : SIGHASH_SINGLE_ANYONECANPAY = SIGHASH_SINGLE_ANYONECANPAY(Int32(bytes))
}


sealed trait SIGHASH_ALL extends HashType
sealed trait SIGHASH_NONE extends HashType
sealed trait SIGHASH_SINGLE extends HashType
sealed trait SIGHASH_ANYONECANPAY extends HashType
sealed trait SIGHASH_ALL_ANYONECANPAY extends HashType
sealed trait SIGHASH_NONE_ANYONECANPAY extends HashType
sealed trait SIGHASH_SINGLE_ANYONECANPAY extends HashType