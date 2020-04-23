package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.p2p.RejectMessage
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.serializers.RawBitcoinSerializer
import scodec.bits.ByteVector

trait RawRejectMessageSerializer extends RawBitcoinSerializer[RejectMessage] {

  def read(bytes: ByteVector): RejectMessage = {
    val messageSize = CompactSizeUInt.parseCompactSizeUInt(bytes)
    val message: String = bytes
      .slice(messageSize.byteSize.toInt,
             messageSize.byteSize.toInt +
               messageSize.num.toInt)
      .toArray
      .map(_.toChar)
      .mkString
    val code: Char = bytes(messageSize.byteSize.toInt + messageSize.num.toInt).toChar
    val reasonSizeStartIndex = messageSize.byteSize.toInt + messageSize.num.toInt + 1
    val reasonSize = CompactSizeUInt.parseCompactSizeUInt(
      bytes.slice(reasonSizeStartIndex.toInt, bytes.size))
    val reason = bytes
      .slice(
        (reasonSizeStartIndex + reasonSize.byteSize).toInt,
        (reasonSizeStartIndex + reasonSize.byteSize.toInt + reasonSize.num.toInt))
      .toArray
      .map(_.toChar)
      .mkString
    val extraStartIndex = (reasonSizeStartIndex + reasonSize.byteSize.toInt + reasonSize.num.toInt)
    val extra = bytes.slice(extraStartIndex, bytes.size)
    RejectMessage(messageSize, message, code, reasonSize, reason, extra)
  }

  def write(rejectMessage: RejectMessage): ByteVector = {
    rejectMessage.messageSize.bytes ++
      ByteVector(rejectMessage.message.map(_.toByte)) ++
      ByteVector.fromByte(rejectMessage.code.toByte) ++
      rejectMessage.reasonSize.bytes ++
      ByteVector(rejectMessage.reason.map(_.toByte)) ++
      rejectMessage.extra
  }
}

object RawRejectMessageSerializer extends RawRejectMessageSerializer
