package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.p2p.HeadersMessage
import scodec.bits.ByteVector

import scala.annotation.tailrec

trait RawHeadersMessageSerializer extends RawBitcoinSerializer[HeadersMessage] {

  def read(bytes: ByteVector): HeadersMessage = {
    val compactSizeUInt = CompactSizeUInt.parseCompactSizeUInt(bytes)
    val headerStartIndex = compactSizeUInt.size.toInt
    val headerBytes = bytes.slice(headerStartIndex, bytes.length)
    val headers = parseBlockHeaders(headerBytes, compactSizeUInt)
    HeadersMessage(compactSizeUInt, headers)
  }

  def write(headersMessage: HeadersMessage): ByteVector = {
    val z = ByteVector.fromByte(0.toByte)
    val headerBytes = headersMessage.headers.foldLeft(ByteVector.empty) {
      case (accum, msg) =>
        accum ++ msg.bytes ++ z
    }
    headersMessage.count.bytes ++ headerBytes
  }

  private def parseBlockHeaders(
      bytes: ByteVector,
      compactSizeUInt: CompactSizeUInt): Vector[BlockHeader] = {
    @tailrec
    def loop(
        remainingBytes: ByteVector,
        remainingHeaders: Long,
        accum: List[BlockHeader]): List[BlockHeader] = {
      if (remainingHeaders <= 0) accum
      //81 is because HeadersMessage appends 0x00 at the end of every block header for some reason
      //read https://bitcoin.org/en/developer-reference#headers
      else {
        require(
          remainingBytes.size >= 80,
          "We do not have enough bytes for another block header, this probably means a tcp frame was not aligned")
        loop(remainingBytes = remainingBytes.slice(81, remainingBytes.length),
             remainingHeaders = remainingHeaders - 1,
             accum = BlockHeader(remainingBytes.take(80)) :: accum)
      }
    }
    loop(bytes, compactSizeUInt.num.toInt, List.empty).reverse.toVector
  }
}

object RawHeadersMessageSerializer extends RawHeadersMessageSerializer
