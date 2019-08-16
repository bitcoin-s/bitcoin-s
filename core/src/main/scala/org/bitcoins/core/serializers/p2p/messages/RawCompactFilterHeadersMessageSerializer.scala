package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.p2p.CompactFilterHeadersMessage
import scodec.bits.ByteVector
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.FilterType
import scala.annotation.tailrec
import org.bitcoins.core.protocol.CompactSizeUInt

object RawCompactFilterHeadersMessageSerializer
    extends RawBitcoinSerializer[CompactFilterHeadersMessage] {

  def read(bytes: ByteVector): CompactFilterHeadersMessage = {
    val filterType = FilterType.fromBytes(bytes.take(1))

    val (stopHashBytes, afterStopHash) = bytes.drop(1).splitAt(32)
    val stopHash = DoubleSha256Digest.fromBytes(stopHashBytes)

    val (previousFilterHeaderBytes, afterPreviousFilterHeader) =
      afterStopHash.splitAt(32)
    val previousFilterHeaderHash =
      DoubleSha256Digest.fromBytes(previousFilterHeaderBytes)

    val filterHashesLength = CompactSizeUInt.parse(afterPreviousFilterHeader)
    require(filterHashesLength.toInt <= 2000)

    val filterHashesBytes = afterPreviousFilterHeader
      .drop(filterHashesLength.bytes.length)
      .take(filterHashesLength.toInt * 32)

    @tailrec
    def loop(
        hashes: Vector[DoubleSha256Digest],
        bytes: ByteVector): (Vector[DoubleSha256Digest], ByteVector) =
      if (bytes.isEmpty) (hashes, ByteVector.empty)
      else {
        val (hashBytes, remainingBytes) = bytes.splitAt(32)
        loop(hashes :+ DoubleSha256Digest.fromBytes(hashBytes), remainingBytes)
      }

    val (hashes, remainingBytes) = loop(Vector.empty, filterHashesBytes)

    require(hashes.length == filterHashesLength.toInt)
    require(remainingBytes.isEmpty)

    val message = CompactFilterHeadersMessage(filterType,
                                              stopHash,
                                              previousFilterHeaderHash,
                                              hashes)

    message
  }

  def write(message: CompactFilterHeadersMessage): ByteVector = {
    import message._
    filterType.bytes ++ stopHash.bytes ++ previousFilterHeader.bytes ++ filterHashesLength.bytes ++ filterHashes
      .foldLeft(ByteVector.empty)(_ ++ _.bytes)
  }

}
