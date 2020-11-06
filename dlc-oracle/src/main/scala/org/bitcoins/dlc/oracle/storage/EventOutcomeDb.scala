package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.protocol.tlv.EnumEventDescriptorV0TLV
import org.bitcoins.crypto.SchnorrNonce
import scodec.bits.ByteVector

case class EventOutcomeDb(
    nonce: SchnorrNonce,
    message: String,
    hashedMessage: ByteVector)

object EventOutcomeDbHelper {

  def createEnumEventDescriptor(
      outcomes: Vector[EventOutcomeDb]): EnumEventDescriptorV0TLV = {
    val strs = outcomes.map(_.message)

    EnumEventDescriptorV0TLV(strs)
  }
}
