package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.serializers.{RawBitcoinSerializer, RawSerializerHelper}
import org.bitcoins.core.p2p.CompactFilterHeadersMessage
import scodec.bits.ByteVector
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.FilterType

import scala.annotation.tailrec
import org.bitcoins.core.protocol.CompactSizeUInt

/**
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#cfheaders BIP157]]
  */
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

    val (hashes, _) = loop(Vector.empty, filterHashesBytes)

    require(hashes.length == filterHashesLength.toInt,
      s"Invalid compact filter headers message: expected number of hashes ${filterHashesLength.toInt}, but got ${hashes.length}")

    val message = CompactFilterHeadersMessage(filterType,
                                              stopHash,
                                              previousFilterHeaderHash,
                                              hashes)

    message
  }

  def write(message: CompactFilterHeadersMessage): ByteVector = {
    val filterType = message.filterType.bytes
    val stopHash = message.stopHash.bytes
    val previousFilterHeader = message.previousFilterHeader.bytes
    val filterHashes = RawSerializerHelper.writeCmpctSizeUInt(message.filterHashes, {fh: DoubleSha256Digest => fh.bytes})

    filterType ++ stopHash ++ previousFilterHeader ++ filterHashes
  }

}
