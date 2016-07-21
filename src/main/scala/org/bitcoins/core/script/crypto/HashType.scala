package org.bitcoins.core.script.crypto

import org.bitcoins.core.number.Int32
import org.bitcoins.core.util.{BitcoinSUtil}

/**
 * Created by chris on 1/18/16.
 */
sealed trait HashType {
  def hashType : Int32
  def hex : String = hashType.hex
}

/**
 * This seems strange, but it needs to take a parameter. This is because
 * SIGHASH_ALL is essentially a catch all if the none of the other hash types are matched.
 * Therefore SIGHASH_ALL could be represented by the byte 0x05 since 0x05 does not match
 * any of the other hash types. The default byte for SIGHASH_ALL is 0x01
 *
 * @param hashType
 */
case class SIGHASH_ALL(hashType: Int32 = Int32.one) extends HashType

case object SIGHASH_NONE extends HashType {
  override def hashType = Int32(2)
}

case object SIGHASH_SINGLE extends HashType {
  override def hashType = Int32(3)
}

case object SIGHASH_ANYONECANPAY extends HashType {
  override def hashType = Int32(80)
}

case object SIGHASH_ALL_ANYONECANPAY extends HashType {
  override def hashType = SIGHASH_ANYONECANPAY.hashType | Int32.one
}

case object SIGHASH_NONE_ANYONECANPAY extends HashType {
  override def hashType = SIGHASH_ANYONECANPAY.hashType | SIGHASH_NONE.hashType
}

case object SIGHASH_SINGLE_ANYONECANPAY extends HashType {
  override def hashType = SIGHASH_ANYONECANPAY.hashType | SIGHASH_SINGLE.hashType
}


