package org.bitcoins.core.p2p

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.serializers.p2p.messages.RawTypeIdentifierSerializer
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

/** This indicates the type of the object that has been hashed for an inventory
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

  /** The hash is of a block header; identical to `MsgBlock`. When used in
    * a `getdata` message, this indicates the response should be a `cmpctblock`
    * message. Only for use in `getdata` messages.
    */
  final case object MsgCompactBlock extends TypeIdentifier {
    val num: UInt32 = UInt32(4)
  }

  /** The hash is a TXID. When used in a `getdata` message, this indicates
    * the response should be a transaction message, if the witness structure
    * is nonempty, the witness serialization will be used. Only for use in
    * `getdata` messages.
    */
  final case object MsgWitnessTx extends TypeIdentifier {

    val num: UInt32 = MsgTx.num | MsgWitnessFlag
  }

  /** The hash is of a block header; identical to `MsgBlock`. When
    * used in a `getdata` message, this indicates the response should
    * be a block message with transactions that have a witness using
    * witness serialization. Only for use in `getdata` messages.
    */
  final case object MsgWitnessBlock extends TypeIdentifier {
    val num: UInt32 = MsgBlock.num | MsgWitnessFlag
  }

  /** Reserved for future use, not used as of Protocol Version 70015.
    */
  final case object MsgFilteredWitnessBlock extends TypeIdentifier {
    val num: UInt32 = MsgFilteredBlock.num | MsgWitnessFlag
  }

  /** from the docs at https://bitcoin.org/en/developer-reference#data-messages
    * These (witness block and tx) are the same as their respective type
    * identifier but with their 30th bit set to indicate witness.
    * For example MSG_WITNESS_TX = 0x01000040.
    */
  private val MsgWitnessFlag = UInt32(1 << 30)

  private case class MsgUnassignedImpl(num: UInt32) extends MsgUnassigned

  override def fromBytes(bytes: ByteVector): TypeIdentifier =
    RawTypeIdentifierSerializer.read(bytes)

  def apply(num: Long): TypeIdentifier = TypeIdentifier(UInt32(num))

  def apply(uInt32: UInt32): TypeIdentifier =
    uInt32 match {
      case UInt32.one               => MsgTx
      case _ if uInt32 == UInt32(2) => MsgBlock
      case _ if uInt32 == UInt32(3) => MsgFilteredBlock
      case x: UInt32                => MsgUnassignedImpl(x)
    }
}
