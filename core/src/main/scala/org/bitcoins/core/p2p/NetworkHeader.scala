package org.bitcoins.core.p2p

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.serializers.p2p.headers.RawNetworkHeaderSerializer
import org.bitcoins.core.util.{CryptoUtil, Factory}
import scodec.bits.ByteVector

/**
  * Represents a message header on the peer-to-peer network
  * @see https://bitcoin.org/en/developer-reference#message-headers
  *
  * @param network Each network has magic bytes indicating the originating network;
                   used to seek to next message when stream state is unknown.
  * @param commandName ASCII string which identifies what message type is contained in the payload.
                       Followed by nulls (0x00) to pad out byte count; for example: version\0\0\0\0\0.
  * @param payloadSize Number of bytes in payload. The current maximum number of bytes (MAX_SIZE) allowed in the payload
  *                    by Bitcoin Core is 32 MiB—messages with a payload size larger than this will be dropped or rejected.
  * @param checksum Added in protocol version 209.
  *                 First 4 bytes of SHA256(SHA256(payload)) in internal byte order.
  *                 If payload is empty, as in verack and getaddr messages,
  *                 the checksum is always 0x5df6e0e2 (SHA256(SHA256(""))).
  */
case class NetworkHeader(
    network: NetworkParameters,
    commandName: String,
    payloadSize: UInt32,
    checksum: ByteVector
) extends NetworkElement {
  require(bytes.length == 24, "NetworkHeaders must be 24 bytes")

  override def bytes: ByteVector = RawNetworkHeaderSerializer.write(this)

}

object NetworkHeader extends Factory[NetworkHeader] {

  override def fromBytes(bytes: ByteVector): NetworkHeader =
    RawNetworkHeaderSerializer.read(bytes)

  /**
    * Creates a network header from it's [[org.bitcoins.core.config.NetworkParameters NetworkParameters]] and [[NetworkPayload]]
    * @param network the [[org.bitcoins.core.config.NetworkParameters NetworkParameters]] object that indicates what network the payload needs to be sent on
    * @param payload the payload object that needs to be sent on the network
    */
  def apply(
      network: NetworkParameters,
      payload: NetworkPayload): NetworkHeader = {
    val checksum = CryptoUtil.doubleSHA256(payload.bytes)
    NetworkHeader(network,
                  payload.commandName,
                  UInt32(payload.bytes.size),
                  checksum.bytes.take(4))
  }
}
