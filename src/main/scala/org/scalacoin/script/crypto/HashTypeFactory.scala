package org.scalacoin.script.crypto

/**
 * Created by chris on 1/18/16.
 */
trait HashTypeFactory {

  def hashTypes = Seq(SIGHASH_ALL,SIGHASH_ANYONECANPAY,SIGHASH_NONE,SIGHASH_SINGLE,
    SIGHASH_NONE_ANYONECANPAY, SIGHASH_ALL_ANYONECANPAY, SIGHASH_SINGLE_ANYONECANPAY)

  def fromString(hex : String) : HashType = {
    //Besides the four listed hashtypes only a hashtype of value 0 appears a few times in the (main)
    //block chain (and is handled like SIGHASH_ALL).
    val hashType = hashTypes.find(_.hex == hex)
    if (hashType.isDefined) hashType.get else SIGHASH_ALL
  }

  def fromByte(byte : Byte) : HashType = {
    val hashType : Option[HashType] = hashTypes.find(_.byte == byte)
    if (hashType.isDefined) hashType.get else SIGHASH_ALL
  }


}

object HashTypeFactory extends HashTypeFactory
