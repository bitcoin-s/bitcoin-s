package org.bitcoins.core.serializers.p2p

import org.bitcoins.core.p2p._
import org.bitcoins.core.serializers.RawBitcoinSerializer
import scodec.bits.ByteVector

trait RawNetworkMessageSerializer extends RawBitcoinSerializer[NetworkMessage] {

  def read(bytes: ByteVector): NetworkMessage = {
    //first 24 bytes are the header
    val (headerBytes,payloadBytes) = bytes.splitAt(24)
    val header = NetworkHeader.fromBytes(headerBytes)
    if (header.payloadSize.toInt > payloadBytes.length) {
      throw new RuntimeException(s"We do not have enough bytes for payload! Expected=${header.payloadSize.toInt} got=${payloadBytes.length}")
    } else {
      val payload = NetworkPayload(header, payloadBytes)
      val n  = NetworkMessage(header, payload)
      n
    }
  }

  def write(networkMessage: NetworkMessage): ByteVector = {
    networkMessage.header.bytes ++ networkMessage.payload.bytes
  }
}

object RawNetworkMessageSerializer extends RawNetworkMessageSerializer
