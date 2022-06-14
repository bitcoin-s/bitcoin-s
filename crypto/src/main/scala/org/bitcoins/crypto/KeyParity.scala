package org.bitcoins.crypto

import scodec.bits.ByteVector

sealed trait KeyParity extends NetworkElement

object KeyParity extends Factory[KeyParity] {

  override def fromBytes(bytes: ByteVector): KeyParity = {
    require(bytes.length == 1, s"Parity must be a single byte, got $bytes")

    bytes.head match {
      case 0x02 => EvenParity
      case 0x03 => OddParity
      case b =>
        throw new IllegalArgumentException(s"Unexpected parity byte: $b")
    }
  }

  def fromByte(byte: Byte): KeyParity = {
    fromBytes(ByteVector.fromByte(byte))
  }
}

case object EvenParity extends KeyParity {
  override val bytes: ByteVector = ByteVector.fromByte(0x02)
}

case object OddParity extends KeyParity {
  override val bytes: ByteVector = ByteVector.fromByte(0x03)
}
