package org.scalacoin.script.crypto

/**
 * Created by chris on 1/18/16.
 */
trait HashTypeFactory {

  def hashTypes = Seq(SIGHASH_ALL,SIGHASH_ANYONECANPAY,SIGHASH_NONE,SIGHASH_SINGLE)

  def fromString(hex : String) : Option[HashType] = {
    //Besides the four listed hashtypes only a hashtype of value 0 appears a few times in the (main)
    //block chain (and is handled like SIGHASH_ALL).
    if (hex == "00") Some(SIGHASH_ALL)
    else hashTypes.find(_.hex == hex)
  }

  def fromByte(byte : Byte) : Option[HashType] = {
    //Besides the four listed hashtypes only a hashtype of value 0 appears a few times in the (main)
    // block chain (and is handled like SIGHASH_ALL).
    if (byte == 0x00) Some(SIGHASH_ALL)
    else hashTypes.find(_.byte == byte)
  }


}

object HashTypeFactory extends HashTypeFactory
