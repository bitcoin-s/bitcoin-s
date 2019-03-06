package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.NetworkElement
import scodec.bits.ByteVector

sealed abstract class ExtKeyVersion extends NetworkElement

object ExtKeyVersion {
  private val all: Seq[ExtKeyVersion] =
    Seq(MainNetPriv, MainNetPub, TestNet3Pub, TestNet3Priv)

  def apply(bytes: ByteVector): Option[ExtKeyVersion] =
    all.find(_.bytes == bytes)

  final case object MainNetPub extends ExtKeyVersion {
    override val bytes = ByteVector(0x04, 0x88, 0xb2, 0x1E)
  }

  final case object MainNetPriv extends ExtKeyVersion {
    override val bytes = ByteVector(0x04, 0x88, 0xAD, 0xE4)
  }

  final case object TestNet3Pub extends ExtKeyVersion {
    override val bytes = ByteVector(0x04, 0x35, 0x87, 0xCF)
  }

  final case object TestNet3Priv extends ExtKeyVersion {
    override val bytes = ByteVector(0x04, 0x35, 0x83, 0x94)
  }
}
