package org.scalacoin.crypto

import org.scalacoin.util.{BitcoinSUtil, ScalacoinUtil}

/**
 * Created by chris on 2/16/16.
 */
trait BaseECKey {

  def hex : String = BitcoinSUtil.encodeHex(bytes)
  def bytes : Seq[Byte]
  def sign(bytes : Seq[Byte]) : Seq[Byte] = ???
  def sign(hex : String) : Seq[Byte] = sign(BitcoinSUtil.decodeHex(hex))
}
