package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.serializers.{RawBitcoinSerializer, RawSerializerHelper}
import org.bitcoins.core.p2p._
import org.bitcoins.crypto.DoubleSha256Digest
import scodec.bits.ByteVector

import scala.annotation.tailrec

/**
  * This trait is responsible for the serialization and deserialization of
  * getblocks messages in on the p2p network
  * @see https://bitcoin.org/en/developer-reference#getblocks
  */
trait RawGetBlocksMessageSerializer
    extends RawBitcoinSerializer[GetBlocksMessage] {

  def read(bytes: ByteVector): GetBlocksMessage = {
    val version = ProtocolVersion(bytes.take(4))
    val hashCount =
      CompactSizeUInt.parseCompactSizeUInt(bytes.slice(4, bytes.size))
    val blockHeaderStartByte = (hashCount.byteSize + 4).toInt
    val blockHeaderBytesStopHash = bytes.slice(blockHeaderStartByte, bytes.size)
    val (blockHashHeaders, remainingBytes) =
      parseBlockHeaders(blockHeaderBytesStopHash, hashCount)
    val stopHash = DoubleSha256Digest(remainingBytes.slice(0, 32))
    GetBlocksMessage(version, hashCount, blockHashHeaders, stopHash)
  }

  def write(getBlocksMessage: GetBlocksMessage): ByteVector = {
    getBlocksMessage.protocolVersion.bytes ++
      getBlocksMessage.hashCount.bytes ++
      RawSerializerHelper.writeNetworkElements(
        getBlocksMessage.blockHeaderHashes) ++
      getBlocksMessage.stopHash.bytes
  }

  /**
    * Helper function to parse block headers from a sequence of bytes
    * Hashes are 32 bytes
    * @param bytes the bytes which need to be parsed into BlockHeader hashes
    * @param compactSizeUInt the p2p network object used to indicate how many block header hashes there are
    * @return the sequence of hashes and the remaining bytes that need to be parsed
    */
  private def parseBlockHeaders(
      bytes: ByteVector,
      compactSizeUInt: CompactSizeUInt): (
      List[DoubleSha256Digest],
      ByteVector) = {
    @tailrec
    def loop(
        remainingHeaders: Long,
        accum: List[DoubleSha256Digest],
        remainingBytes: ByteVector): (List[DoubleSha256Digest], ByteVector) = {
      if (remainingHeaders <= 0) (accum.reverse, remainingBytes)
      else {
        val dsha256 = DoubleSha256Digest(remainingBytes.slice(0, 32))
        val rem = remainingBytes.slice(32, remainingBytes.size)
        loop(remainingHeaders = remainingHeaders - 1,
             accum = dsha256 :: accum,
             remainingBytes = rem)
      }
    }
    loop(compactSizeUInt.num.toInt, List.empty, bytes)
  }
}

object RawGetBlocksMessageSerializer extends RawGetBlocksMessageSerializer
