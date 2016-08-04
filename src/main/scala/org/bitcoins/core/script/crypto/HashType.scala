package org.bitcoins.core.script.crypto

import org.bitcoins.core.number.Int32
import org.bitcoins.core.script.crypto.SIGHASH_ALL.SIGHASH_ALLImpl
import org.bitcoins.core.util.{Factory, BitcoinSUtil}

/**
 * Created by chris on 1/18/16.
 */
sealed trait HashType {
  type HashType
  def num : Int32
}

trait HashTypeOperations extends Factory[HashType] {
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
      if (isSIGHASH_ALL_ANYONECANPAY(num)) SIGHASH_ALL_ANYONECANPAY(num) else SIGHASH_ANYONECANPAY(num)
    }
    else {
      SIGHASH_ALL(num)
    }
  }
  def isSIGHASH_ALL_ONE(num : Int32) : Boolean = (num & Int32(0x1f)) == Int32(1)
  def isSIGHASH_NONE(num : Int32) : Boolean = (num & Int32(0x1f)) == Int32(2)
  def isSIGHASH_SINGLE(num : Int32) : Boolean = (num & Int32(0x1f)) == Int32(3)
  def isSIGHASH_ANYONECANPAY(num : Int32) : Boolean = (num & Int32(0x80)) == Int32(0x80)
  def isSIGHASH_ALL_ANYONECANPAY(num : Int32) : Boolean = isSIGHASH_ALL_ONE(num) && isSIGHASH_ANYONECANPAY(num)
  def isSIGHASH_NONE_ANYONECANPAY(num : Int32) : Boolean = isSIGHASH_NONE(num) && isSIGHASH_ANYONECANPAY(num)
  def isSIGHASH_SINGLE_ANYONECANPAY(num: Int32) : Boolean = isSIGHASH_SINGLE(num) && isSIGHASH_ANYONECANPAY(num)

  val hashTypes = Seq(SIGHASH_ALL.value, SIGHASH_NONE, SIGHASH_SINGLE, SIGHASH_ANYONECANPAY,
    SIGHASH_NONE_ANYONECANPAY, SIGHASH_ALL_ANYONECANPAY, SIGHASH_SINGLE_ANYONECANPAY)

}

object HashTypeOperations extends HashTypeOperations

/**
 * This seems strange, but it needs to take a parameter. This is because
 * SIGHASH_ALL is essentially a catch all if the none of the other hash types are matched.
 * Therefore SIGHASH_ALL could be represented by the byte 0x05 since 0x05 does not match
 * any of the other hash types. The default byte for SIGHASH_ALL is 0x01
 *
 */

 object SIGHASH_ALL extends Factory[HashType] with HashTypeOperations {
  private case class SIGHASH_ALLImpl(num: Int32) extends SIGHASH_ALL
  def value : HashType = SIGHASH_ALL(Int32.one)
  def apply(num : Int32) : HashType = SIGHASH_ALLImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : HashType = SIGHASH_ALL(bytes)
}

 object SIGHASH_NONE extends Factory[HashType] with HashTypeOperations{
  private case class SIGHASH_NONEImpl(num : Int32) extends SIGHASH_NONE {
    require(isSIGHASH_NONE(num), "The bitwise AND of 'num & 0x1f' must be 2 for a hashtype of SIGHASH_NONE.")
  }
  def value : HashType= SIGHASH_NONE(Int32(2))
  def apply(num : Int32) : HashType = SIGHASH_NONEImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : HashType = SIGHASH_NONE(bytes)
}

object SIGHASH_SINGLE extends Factory[HashType] with HashTypeOperations{
  private case class SIGHASH_SINGLEImpl(num : Int32) extends SIGHASH_SINGLE {
    require(isSIGHASH_SINGLE(num), "The bitwise AND of 'num & 0x1f' must be 3 for a hashtype of SIGHASH_SINGLE.")
  }
  def value : HashType = SIGHASH_SINGLE(Int32(3))
  def apply(num : Int32) : HashType = SIGHASH_SINGLEImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : HashType = SIGHASH_SINGLE(bytes)
}

object SIGHASH_ANYONECANPAY extends Factory[HashType] with HashTypeOperations{
  private case class SIGHASH_ANYONECANPAYImpl(num : Int32) extends SIGHASH_ANYONECANPAY {
    require(isSIGHASH_ANYONECANPAY(num), "The bitwise AND of 'num & 0x80' must be 0x80 (or 128) for a hashtype of SIGHASH_ANYONECANPAY.")
  }
  def value : HashType = SIGHASH_ANYONECANPAY(Int32(0x80))
  def apply(num : Int32) : HashType = SIGHASH_ANYONECANPAYImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : HashType = SIGHASH_ANYONECANPAY(bytes)
}

object SIGHASH_ALL_ANYONECANPAY extends Factory[HashType] with HashTypeOperations{
  private case class SIGHASH_ALL_ANYONECANPAYImpl(num : Int32) extends SIGHASH_ALL_ANYONECANPAY {
    require(isSIGHASH_ALL_ANYONECANPAY(num), "SIGHASH_ALL_ANYONECANPAY must be of both hashTypes: SIGHASH_ALL, and SIGHASH_ANYONECANPAY.")
  }
  def value : HashType = SIGHASH_ALL_ANYONECANPAY(SIGHASH_ANYONECANPAY.value.num | SIGHASH_ALL.value.num)
  def apply(num : Int32) : HashType = SIGHASH_ALL_ANYONECANPAYImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : HashType = SIGHASH_ALL_ANYONECANPAY(bytes)
}

object SIGHASH_NONE_ANYONECANPAY extends Factory[HashType] with HashTypeOperations{
  private case class SIGHASH_NONE_ANYONECANPAYImpl(num : Int32) extends SIGHASH_NONE_ANYONECANPAY {
    require(isSIGHASH_NONE_ANYONECANPAY(num), "SIGHASH_NONE_ANYONECANPAY must be of both hashTypes: SIGHASH_NONE, and SIGHASH_ANYONECANPAY.")
  }
  def value : HashType = SIGHASH_NONE_ANYONECANPAY(SIGHASH_ANYONECANPAY.value.num | SIGHASH_NONE.value.num)
  def apply(num : Int32) : HashType = SIGHASH_NONE_ANYONECANPAYImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : HashType = SIGHASH_NONE_ANYONECANPAY(bytes)
}

object SIGHASH_SINGLE_ANYONECANPAY extends Factory[HashType] with HashTypeOperations{
  private case class SIGHASH_SINGLE_ANYONECANPAYImpl(num : Int32) extends SIGHASH_SINGLE_ANYONECANPAY {
    require(isSIGHASH_SINGLE_ANYONECANPAY(num), "SIGHASH_SINGLE_ANYONECANPAY must be of both hashTypes: SIGHASH_SINGLE, and SIGHASH_ANYONECANPAY.")
  }
  def value : HashType = SIGHASH_SINGLE_ANYONECANPAY(SIGHASH_ANYONECANPAY.value.num | SIGHASH_SINGLE.value.num)
  def apply(num : Int32) : HashType = SIGHASH_SINGLE_ANYONECANPAYImpl(num)
  override def fromBytes (bytes : Seq[Byte]) : HashType = SIGHASH_SINGLE_ANYONECANPAY(bytes)
}


sealed trait SIGHASH_ALL extends HashType
sealed trait SIGHASH_NONE extends HashType
sealed trait SIGHASH_SINGLE extends HashType
sealed trait SIGHASH_ANYONECANPAY extends HashType
sealed trait SIGHASH_ALL_ANYONECANPAY extends HashType
sealed trait SIGHASH_NONE_ANYONECANPAY extends HashType
sealed trait SIGHASH_SINGLE_ANYONECANPAY extends HashType