package org.bitcoins.script.crypto

import org.bitcoins.util.{BitcoinSUtil}

/**
 * Created by chris on 1/18/16.
 */
sealed trait HashType {
  def byte : Byte
  def hex : String = BitcoinSUtil.encodeHex(byte)
}

/**
 * This seems strange, but it needs to take a parameter. This is because
 * SIGHASH_ALL is essentially a catch all if the none of the other hash types are matched.
 * Therefore SIGHASH_ALL could be represented by the byte 0x05 since 0x05 does not match
 * any of the other hash types. The default byte for SIGHASH_ALL is 0x01
 *
 * @param byte
 */
case class SIGHASH_ALL(byte : Byte = 0x01.toByte) extends HashType

case object SIGHASH_NONE extends HashType {
  override def byte = 0x02.toByte
}

case object SIGHASH_SINGLE extends HashType {
  override def byte = 0x03.toByte
}

case object SIGHASH_ANYONECANPAY extends HashType {
  override def byte = 0x80.toByte
}

case object SIGHASH_ALL_ANYONECANPAY extends HashType {
  override def byte = (SIGHASH_ANYONECANPAY.byte | 0x01.toByte).toByte
}

case object SIGHASH_NONE_ANYONECANPAY extends HashType {
  override def byte = (SIGHASH_ANYONECANPAY.byte | SIGHASH_NONE.byte).toByte
}

case object SIGHASH_SINGLE_ANYONECANPAY extends HashType {
  override def byte = (SIGHASH_ANYONECANPAY.byte | SIGHASH_SINGLE.byte).toByte
}


