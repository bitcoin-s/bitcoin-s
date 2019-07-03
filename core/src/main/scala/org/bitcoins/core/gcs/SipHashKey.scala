package org.bitcoins.core.gcs
import scodec.bits.ByteVector
import org.bitcoins.core.protocol.NetworkElement
case class SipHashKey(bytes: ByteVector) extends NetworkElement {
  require(bytes.size == 16,
          "Can only use a key length of 16 bytes, got: " + bytes.size)
}
