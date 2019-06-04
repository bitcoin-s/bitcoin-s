package org.bitcoins.node.messages.data

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.Factory
import org.bitcoins.node.messages.HeadersMessage
import org.bitcoins.node.serializers.messages.data.RawHeadersMessageSerializer
import org.bitcoins.node.messages.HeadersMessage
import scodec.bits.ByteVector

/**
  * Created by chris on 7/5/16.
  */
object HeadersMessage extends Factory[HeadersMessage] {
  private case class HeadersMessageImpl(
      count: CompactSizeUInt,
      headers: Vector[BlockHeader])
      extends HeadersMessage

  def fromBytes(bytes: ByteVector): HeadersMessage =
    RawHeadersMessageSerializer.read(bytes)

  def apply(
      count: CompactSizeUInt,
      headers: Vector[BlockHeader]): HeadersMessage =
    HeadersMessageImpl(count, headers)

  def apply(headers: Vector[BlockHeader]): HeadersMessage = {
    val count = CompactSizeUInt(UInt64(headers.length))
    HeadersMessageImpl(count, headers)
  }
}
