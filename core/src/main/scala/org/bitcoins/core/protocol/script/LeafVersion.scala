package org.bitcoins.core.protocol.script

sealed abstract class LeafVersion {
  def toByte: Byte
}

object LeafVersion {

  /** BIP342 specifies validity rules that apply for leaf version 0xc0, but
    * future proposals can introduce rules for other leaf versions.
    *
    * @see
    *   https://github.com/bitcoin/bips/blob/master/bip-0341.mediawiki#rationale
    */
  case object Tapscript extends LeafVersion {
    override def toByte: Byte = 0xc0.toByte
  }

  case class UnknownLeafVersion(toByte: Byte) extends LeafVersion

  val knownLeafVersions: Vector[LeafVersion] = Vector(Tapscript)

  final val TAPROOT_LEAF_MASK: Byte = 0xfe.toByte

  def fromByte(byte: Byte): LeafVersion = {
    knownLeafVersions
      .find(_.toByte == byte)
      .getOrElse(UnknownLeafVersion(byte))
  }

  def fromMaskedByte(byte: Byte): LeafVersion = {
    fromByte((TAPROOT_LEAF_MASK & byte).toByte)
  }
}
