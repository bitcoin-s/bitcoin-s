package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.serializers.{RawBitcoinSerializer, RawSerializerHelper}
import org.bitcoins.crypto.DoubleSha256Digest
import scodec.bits.ByteVector

import scala.annotation.tailrec

trait RawGetHeadersMessageSerializer
    extends RawBitcoinSerializer[GetHeadersMessage] {

  override def read(bytes: ByteVector): GetHeadersMessage = {
    val version = ProtocolVersion(bytes.take(4))
    val hashCount =
      CompactSizeUInt.parseCompactSizeUInt(bytes.slice(4, bytes.length))
    val hashesStartIndex = (hashCount.byteSize + 4).toInt
    val (hashes, remainingBytes) =
      parseHashes(bytes.drop(hashesStartIndex), hashCount)
    val hashStop = DoubleSha256Digest(remainingBytes.take(32))
    GetHeadersMessage(version, hashCount, hashes.toVector, hashStop)
  }

  override def write(getHeadersMessage: GetHeadersMessage): ByteVector = {
    getHeadersMessage.version.bytes ++
      getHeadersMessage.hashCount.bytes ++
      RawSerializerHelper.writeNetworkElements(getHeadersMessage.hashes) ++
      getHeadersMessage.hashStop.bytes
  }

  /** Parses hashes inside of [[GetHeadersMessage]]
    *
    * @param bytes
    *   the bytes which the hashes are parsed from
    * @param numHashes
    *   the number of hases that need to be parsed
    * @return
    *   the parsed hases and the remaining bytes in the network message
    */
  private def parseHashes(
      bytes: ByteVector,
      numHashes: CompactSizeUInt): (List[DoubleSha256Digest], ByteVector) = {
    @tailrec
    def loop(
        remainingBytes: ByteVector,
        remainingHashes: Long,
        accum: List[DoubleSha256Digest])
        : (List[DoubleSha256Digest], ByteVector) = {
      if (remainingHashes <= 0) (accum, remainingBytes)
      else {
        val hash = DoubleSha256Digest(remainingBytes.take(32))
        loop(remainingBytes.drop(32), remainingHashes - 1, accum :+ hash)
      }
    }

    loop(bytes, numHashes.num.toInt, List.empty)
  }
}

object RawGetHeadersMessageSerializer extends RawGetHeadersMessageSerializer
