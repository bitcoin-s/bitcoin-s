package org.bitcoins.core.serializers.p2p

import org.bitcoins.core.p2p._
import org.bitcoins.core.serializers.RawBitcoinSerializer
import scodec.bits.ByteVector

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
