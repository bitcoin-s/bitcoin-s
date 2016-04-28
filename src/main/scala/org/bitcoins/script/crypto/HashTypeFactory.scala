package org.bitcoins.script.crypto

import org.bitcoins.util.BitcoinSUtil

/**
 * Created by chris on 1/18/16.
 */
trait HashTypeFactory {

  def hashTypes = Seq(SIGHASH_ALL(0x01.toByte),SIGHASH_ANYONECANPAY,SIGHASH_NONE,SIGHASH_SINGLE,
    SIGHASH_NONE_ANYONECANPAY, SIGHASH_ALL_ANYONECANPAY, SIGHASH_SINGLE_ANYONECANPAY)

  def fromString(hex : String) : HashType = {
    require(hex.size == 2,"We cannot create a hash type out of a string not equal to 2 chars in size")
    //Besides the four listed hashtypes only a hashtype of value 0 appears a few times in the (main)
    //block chain (and is handled like SIGHASH_ALL).
    val hashType = hashTypes.find(_.hex == hex)
    if (hashType.isDefined) hashType.get else SIGHASH_ALL(BitcoinSUtil.hexToLong(hex).toByte)
  }

  def fromByte(byte : Byte) : HashType = {
    val hashType : Option[HashType] = hashTypes.find(_.byte == byte)
    if (hashType.isDefined) hashType.get else SIGHASH_ALL(byte)
  }


}

object HashTypeFactory extends HashTypeFactory
