package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.BitcoinSUtil

sealed abstract class ExtKeyVersion extends NetworkElement {
  def bytes: scodec.bits.ByteVector
  override def hex = BitcoinSUtil.encodeHex(bytes)
}

case object MainNetPub extends ExtKeyVersion {
  override def bytes = scodec.bits.ByteVector(0x04, 0x88, 0xb2, 0x1E)
}

case object MainNetPriv extends ExtKeyVersion {
  override def bytes = scodec.bits.ByteVector(0x04, 0x88, 0xAD, 0xE4)
}

case object TestNet3Pub extends ExtKeyVersion {
  override def bytes = scodec.bits.ByteVector(0x04, 0x35, 0x87, 0xCF)
}

case object TestNet3Priv extends ExtKeyVersion {
  override def bytes = scodec.bits.ByteVector(0x04, 0x35, 0x83, 0x94)
}

object ExtKeyVersion {
  private val all: Seq[ExtKeyVersion] = Seq(MainNetPriv, MainNetPub, TestNet3Pub, TestNet3Priv)

  def apply(bytes: scodec.bits.ByteVector): Option[ExtKeyVersion] = all.find(_.bytes == bytes)
}
