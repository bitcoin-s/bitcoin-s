package org.scalacoin.crypto

import org.scalacoin.util.BitcoinSUtil

/**
 * Created by chris on 2/26/16.
 */
trait ECDigitalSignature {

  def hex : String = BitcoinSUtil.encodeHex(bytes)
  def bytes : Seq[Byte]
}

case class ECDigitalSignatureImpl(bytes : Seq[Byte]) extends ECDigitalSignature
