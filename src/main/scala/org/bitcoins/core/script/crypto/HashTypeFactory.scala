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

/*  def fromString(hex : String) : HashType = {
    //Besides the four listed hashtypes only a hashtype of value 0 appears a few times in the (main)
    //block chain (and is handled like SIGHASH_ALL).
    val hashType = hashTypes.find(_.hex == hex)
    if (hashType.isDefined) hashType.get else SIGHASH_ALL(ScriptNumberUtil.toLong(hex))
  }*/

  def fromBytes(bytes : Seq[Byte]) : HashType = {
    val num = Int32(bytes.toArray)
    val hashType : Option[HashType] = hashTypes.find(_.hashType == num)
    if (hashType.isDefined) hashType.get else SIGHASH_ALL(num)
  }

}

object HashTypeFactory extends HashTypeFactory
