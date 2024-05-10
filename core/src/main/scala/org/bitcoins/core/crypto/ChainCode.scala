package org.bitcoins.core.crypto

import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

case class ChainCode(bytes: ByteVector) extends NetworkElement {
  require(bytes.size == 32,
          "ChainCode must be 32 bytes in size, got: " + bytes.size)
}

object ChainCode extends Factory[ChainCode] {

  def fromBytes(bytes: ByteVector): ChainCode =
    // use new to avoid inf loop
    new ChainCode(bytes)
}
