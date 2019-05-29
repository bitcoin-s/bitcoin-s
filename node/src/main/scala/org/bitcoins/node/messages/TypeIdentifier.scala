package org.bitcoins.node.messages

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import org.bitcoins.node.serializers.messages.RawTypeIdentifierSerializer
import org.bitcoins.node.serializers.messages.RawTypeIdentifierSerializer
import scodec.bits.ByteVector

/**
  * This indicates the type of the object that has been hashed for an inventory

  * @see https://bitcoin.org/en/developer-reference#data-messages
  */
sealed trait TypeIdentifier extends NetworkElement {
  def num: UInt32
  override def bytes: ByteVector = RawTypeIdentifierSerializer.write(this)
}

sealed trait MsgUnassigned extends TypeIdentifier

object TypeIdentifier extends Factory[TypeIdentifier] {

  /** The corresponding hash is a TXID */
  final case object MsgTx extends TypeIdentifier {
    override val num = UInt32.one
  }

  /** The corresponding hash is a block header */
  final case object MsgBlock extends TypeIdentifier {
    override val num = UInt32(2)
  }

  /**
    * The corresponding hash is a block header
    * When used in a `getdata` message, this indicates
    * the response should be a merkleblock message
    * rather than a block message (but this only works
    * if a bloom filter was previously configured).
    */
  final case object MsgFilteredBlock extends TypeIdentifier {
    override val num = UInt32(3)
  }

  private case class MsgUnassignedImpl(num: UInt32) extends MsgUnassigned

  override def fromBytes(bytes: ByteVector): TypeIdentifier =
    RawTypeIdentifierSerializer.read(bytes)

  def apply(num: Long): TypeIdentifier = TypeIdentifier(UInt32(num))

  def apply(uInt32: UInt32): TypeIdentifier = uInt32 match {
    case MsgTx.num            => MsgTx
    case MsgBlock.num         => MsgBlock
    case MsgFilteredBlock.num => MsgFilteredBlock
    case x: UInt32            => MsgUnassignedImpl(x)
  }
}
