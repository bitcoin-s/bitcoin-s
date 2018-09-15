package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import scodec.bits.ByteVector

class ShortChannelId(u64: UInt64) extends NetworkElement {
  override def bytes: ByteVector = u64.bytes
}

object ShortChannelId extends Factory[ShortChannelId] {

  override def fromBytes(byteVector: ByteVector): ShortChannelId = {
    new ShortChannelId(UInt64.fromBytes(byteVector))
  }
}
