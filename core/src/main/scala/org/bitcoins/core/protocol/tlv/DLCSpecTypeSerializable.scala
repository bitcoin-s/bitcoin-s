package org.bitcoins.core.protocol.tlv

import org.bitcoins.crypto.NetworkElement
import scodec.bits.ByteVector

trait DLCSpecTypeSerializable[+T <: DLCSpecType] extends NetworkElement {
  def toSubType: T

  override def bytes: ByteVector = toSubType.bytes
}
