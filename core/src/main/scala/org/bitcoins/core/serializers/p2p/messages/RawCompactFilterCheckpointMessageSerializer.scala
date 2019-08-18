package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.p2p.CompactFilterCheckPointMessage
import org.bitcoins.core.protocol.CompactSizeUInt
import scodec.bits.ByteVector

import scala.annotation.tailrec

object RawCompactFilterCheckpointMessageSerializer
    extends RawBitcoinSerializer[CompactFilterCheckPointMessage] {
  def read(bytes: ByteVector): CompactFilterCheckPointMessage = {
    val filterType = FilterType.fromBytes(bytes.take(1))

    val (stopHashBytes, afterStopHash) = bytes.drop(1).splitAt(32)
    val stopHash = DoubleSha256Digest.fromBytes(stopHashBytes)

    val filterHeadersLength = CompactSizeUInt.parse(afterStopHash)
    require(filterHeadersLength.toInt <= 2000)

    val filterHeadersBytes = afterStopHash
      .drop(filterHeadersLength.bytes.length)
      .take(filterHeadersLength.toInt * 32)

    @tailrec
    def loop(hashes: Vector[DoubleSha256Digest], bytes: ByteVector): (Vector[DoubleSha256Digest], ByteVector) =
      if (bytes.isEmpty) (hashes, ByteVector.empty)
      else {
        val (hashBytes, remainingBytes) = bytes.splitAt(32)
        loop(hashes :+ DoubleSha256Digest.fromBytes(hashBytes), remainingBytes)
      }

    val (headers, remainingBytes) = loop(Vector.empty, filterHeadersBytes)

    require(headers.length == filterHeadersLength.toInt)
    require(remainingBytes.isEmpty)

    CompactFilterCheckPointMessage(filterType,
      stopHash,
      headers)
  }

  def write(message: CompactFilterCheckPointMessage): ByteVector = {
    message.filterType.bytes ++ message.stopHash.bytes ++ message.filterHeadersLength.bytes ++
      message.filterHeaders.foldLeft(ByteVector.empty)(_ ++ _.bytes)
  }
}
