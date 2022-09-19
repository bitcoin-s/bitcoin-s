package org.bitcoins.core.protocol.tlv

import org.bitcoins.crypto.Factory
import scodec.bits.ByteVector

object SubTypeUtil {

  /** Parses the first byte as a sub type, then looks into the
    * map and finds the associated factory and parses to a T
    */
  def fromBytes[T <: DLCSubType](
      bytes: ByteVector,
      subTypeMap: Map[Byte, Factory[T]]): T = {
    require(bytes.nonEmpty,
            s"Cannot have empty byte vector for parsing subtype")
    val subType = bytes.head
    val factoryOpt = subTypeMap.get(subType)
    val descriptorOpt: Option[T] = {
      factoryOpt.map(_.fromBytes(bytes))
    }
    descriptorOpt match {
      case Some(descriptor) => descriptor
      case None =>
        sys.error(s"Cannot parse bytes, got=$bytes")
    }
  }
}
