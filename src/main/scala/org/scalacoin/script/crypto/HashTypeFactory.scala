package org.scalacoin.script.crypto

/**
 * Created by chris on 1/18/16.
 */
trait HashTypeFactory {

  def hashTypes = Seq(SIGHASH_ALL,SIGHASH_ANYONECANPAY,SIGHASH_NONE,SIGHASH_SINGLE)

  def fromString(hex : String) : Option[HashType] = hashTypes.find(_.hex == hex)

  def fromByte(byte : Byte) : Option[HashType] = hashTypes.find(_.byte == byte)

}

object HashTypeFactory extends HashTypeFactory
