package org.scalacoin.script.crypto

import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/18/16.
 */
sealed trait HashType {
  def byte : Byte = ScalacoinUtil.decodeHex(hex).head
  def hex : String
}

case object SIGHASH_ALL extends HashType {
  override def hex = "01"
}

case object SIGHASH_NONE extends HashType {
  override def hex = "02"
}

case object SIGHASH_SINGLE extends HashType {
  override def hex = "03"
}

case object SIGHASH_ANYONECANPAY extends HashType {
  override def hex = "80"
}
