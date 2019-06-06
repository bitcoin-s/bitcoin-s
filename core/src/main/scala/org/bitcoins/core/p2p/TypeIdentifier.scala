package org.bitcoins.core.p2p

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.serializers.p2p.messages.RawTypeIdentifierSerializer
import org.bitcoins.core.util.Factory
import scodec.bits.ByteVector

/**
  * This indicates the type of the object that has been hashed for an inventory
  *
  * @see https://bitcoin.org/en/developer-reference#data-messages
  */
sealed trait TypeIdentifier extends NetworkElement {
  def num: UInt32
  override def bytes: ByteVector = RawTypeIdentifierSerializer.write(this)
}

sealed trait MsgUnassigned extends TypeIdentifier

object TypeIdentifier extends Factory[TypeIdentifier] {

  final case object MsgTx extends TypeIdentifier {
    override val num = UInt32.one
  }

  final case object MsgBlock extends TypeIdentifier {
    override val num = UInt32(2)
  }

  final case object MsgFilteredBlock extends TypeIdentifier {
    override val num = UInt32(3)
  }

  private case class MsgUnassignedImpl(num: UInt32) extends MsgUnassigned

  override def fromBytes(bytes: ByteVector): TypeIdentifier =
    RawTypeIdentifierSerializer.read(bytes)

  def apply(num: Long): TypeIdentifier = TypeIdentifier(UInt32(num))

  def apply(uInt32: UInt32): TypeIdentifier = uInt32 match {
    case UInt32.one                 => MsgTx
    case _ if (uInt32 == UInt32(2)) => MsgBlock
    case _ if (uInt32 == UInt32(3)) => MsgFilteredBlock
    case x: UInt32                  => MsgUnassignedImpl(x)
  }
}
