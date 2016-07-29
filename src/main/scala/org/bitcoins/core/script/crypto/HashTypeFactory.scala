package org.bitcoins.core.script.crypto

import org.bitcoins.core.number.Int32
import org.bitcoins.core.script.constant.ScriptNumberUtil
import org.bitcoins.core.util.Factory

/**
 * Created by chris on 1/18/16.
 */
trait HashTypeFactory extends Factory[HashType] {

  def hashTypes = Seq(SIGHASH_ALL(Int32.one),SIGHASH_ANYONECANPAY,SIGHASH_NONE,SIGHASH_SINGLE,
    SIGHASH_NONE_ANYONECANPAY, SIGHASH_ALL_ANYONECANPAY, SIGHASH_SINGLE_ANYONECANPAY)

  def fromBytes(bytes : Seq[Byte]) : HashType = {
    val num = Int32(bytes)
    fromNumber(num)
  }

  def fromNumber(num : Int32) : HashType = {
    if (isSIGHASH_NONE(num)) {
      if (isSIGHASH_ANYONECANPAY(num)) SIGHASH_NONE_ANYONECANPAY else SIGHASH_NONE
    }
    else if (isSIGHASH_SINGLE(num)) {
      if (isSIGHASH_ANYONECANPAY(num)) SIGHASH_SINGLE_ANYONECANPAY else SIGHASH_SINGLE
    }
    else if (isSIGHASH_ANYONECANPAY(num)) {
      if (isSIGHASH_ALL_ONE(num)) SIGHASH_ALL_ANYONECANPAY else SIGHASH_ANYONECANPAY
    }
    else {
      SIGHASH_ALL(num)
    }
  }

  private def isSIGHASH_ANYONECANPAY(num : Int32) : Boolean = (num & Int32(0x80)) == SIGHASH_ANYONECANPAY.hashType
  private def isSIGHASH_SINGLE(num : Int32) : Boolean = (num & Int32(0x1f)) == SIGHASH_SINGLE.hashType
  private def isSIGHASH_NONE(num : Int32) : Boolean = (num & Int32(0x1f)) == SIGHASH_NONE.hashType
  private def isSIGHASH_ALL_ONE(num : Int32) : Boolean = (num & Int32(0x1f)) == SIGHASH_ALL(Int32.one).hashType


}

object HashTypeFactory extends HashTypeFactory
