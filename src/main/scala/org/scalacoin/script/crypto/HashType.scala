package org.scalacoin.script.crypto

import org.scalacoin.util.{BitcoinSUtil, ScalacoinUtil}

/**
 * Created by chris on 1/18/16.
 */
sealed trait HashType {
  def byte : Byte
  def hex : String = BitcoinSUtil.encodeHex(byte)
}

case object SIGHASH_ALL extends HashType {
  override def byte = 0x01.toByte
}

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
  override def byte = (SIGHASH_ANYONECANPAY.byte | SIGHASH_ALL.byte).toByte
}

case object SIGHASH_NONE_ANYONECANPAY extends HashType {
  override def byte = (SIGHASH_ANYONECANPAY.byte | SIGHASH_NONE.byte).toByte
}

case object SIGHASH_SINGLE_ANYONECANPAY extends HashType {
  override def byte = (SIGHASH_ANYONECANPAY.byte | SIGHASH_SINGLE.byte).toByte
}


