package org.bitcoins.node.serializers

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.headers.NetworkHeader
import org.bitcoins.node.messages.NetworkPayload
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.headers.NetworkHeader
import scodec.bits.ByteVector

/**
  * Created by chris on 6/11/16.
  */
trait RawNetworkMessageSerializer extends RawBitcoinSerializer[NetworkMessage] {

  def read(bytes: ByteVector): NetworkMessage = {
    //first 24 bytes are the header
    val header = NetworkHeader(bytes.take(24))
    val payload = NetworkPayload(header, bytes.slice(24, bytes.size))
    NetworkMessage(header, payload)
  }

  def write(networkMessage: NetworkMessage): ByteVector = {
    networkMessage.header.bytes ++ networkMessage.payload.bytes
  }
}

object RawNetworkMessageSerializer extends RawNetworkMessageSerializer
