package org.bitcoins.node.messages.data

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.util.Factory
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.messages.GetHeadersMessage
import org.bitcoins.node.serializers.messages.data.RawGetHeadersMessageSerializer
import org.bitcoins.node.versions.ProtocolVersion
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.messages.GetHeadersMessage
import org.bitcoins.node.serializers.messages.data.RawGetHeadersMessageSerializer
import org.bitcoins.node.versions.ProtocolVersion
import scodec.bits.ByteVector

/**
  * Created by chris on 6/29/16.
  */
object GetHeadersMessage extends Factory[GetHeadersMessage] {
  private case class GetHeadersMessageImpl(
      version: ProtocolVersion,
      hashCount: CompactSizeUInt,
      hashes: Seq[DoubleSha256Digest],
      hashStop: DoubleSha256Digest)
      extends GetHeadersMessage

  override def fromBytes(bytes: ByteVector): GetHeadersMessage =
    RawGetHeadersMessageSerializer.read(bytes)

  def apply(
      version: ProtocolVersion,
      hashCount: CompactSizeUInt,
      hashes: Seq[DoubleSha256Digest],
      hashStop: DoubleSha256Digest): GetHeadersMessage = {
    GetHeadersMessageImpl(version, hashCount, hashes, hashStop)
  }

  def apply(
      version: ProtocolVersion,
      hashes: Seq[DoubleSha256Digest],
      hashStop: DoubleSha256Digest): GetHeadersMessage = {
    val hashCount = CompactSizeUInt(UInt64(hashes.length))
    GetHeadersMessage(version, hashCount, hashes, hashStop)
  }

  /** Creates a [[GetHeadersMessage]] with the default protocol version in [[Constants]] */
  def apply(
      hashes: Seq[DoubleSha256Digest],
      hashStop: DoubleSha256Digest): GetHeadersMessage = {
    GetHeadersMessage(Constants.version, hashes, hashStop)
  }

  /** Creates a [[GetHeadersMessage]] with no hash stop set, this requests all possible blocks
    * if we need more than 2000 block headers, we will have to send another [[GetHeadersMessage]]
    * [[https://bitcoin.org/en/developer-reference#getheaders]] */
  def apply(hashes: Seq[DoubleSha256Digest]): GetHeadersMessage = {
    //The header hash of the last header hash being requested; set to all zeroes to request an inv message with all
    //subsequent header hashes (a maximum of 2000 will be sent as a reply to this message
    val hashStop = DoubleSha256Digest.empty
    GetHeadersMessage(hashes, hashStop)
  }

  def apply(hashes: DoubleSha256Digest): GetHeadersMessage = {
    GetHeadersMessage(Vector(hashes))
  }
}
