package org.scalacoin.crypto

import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 2/16/16.
 */
trait BaseECKey {

  def hex : String
  def bytes : Seq[Byte]
  def sign(bytes : Seq[Byte]) : Seq[Byte] = ???
  def sign(hex : String) : Seq[Byte] = sign(ScalacoinUtil.decodeHex(hex))
}
