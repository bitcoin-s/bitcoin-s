package org.bitcoins.core.protocol.tlv

import org.bitcoins.crypto.Factory
import scodec.bits.ByteVector

abstract class DLCSpecTypeDeserializable[
    T <: DLCSpecType,
    +U <: DLCSpecTypeSerializable[T]](subTypeFactory: Factory[T])
    extends Factory[U] {
  def fromSubType(tlv: T): U

  override def fromBytes(bytes: ByteVector): U = {
    fromSubType(subTypeFactory.fromBytes(bytes))
  }
}
