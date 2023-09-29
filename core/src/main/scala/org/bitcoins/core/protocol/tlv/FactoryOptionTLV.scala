package org.bitcoins.core.protocol.tlv

import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

trait FactoryOptionTLV[T <: NetworkElement] extends Factory[OptionDLCType[T]] {

  override def fromBytes(bytes: ByteVector): OptionDLCType[T]
}
