package org.bitcoins.core.protocol.tlv

import org.bitcoins.core.number.UInt16
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

case class LnMessage[+T <: TLV](tlv: T) extends NetworkElement {
  require(tlv.tpe.toLong <= 65535L, s"LN Message format requires UInt16 types")
  val tpe: UInt16 = UInt16(tlv.tpe.toInt)
  val payload: ByteVector = tlv.value
  override lazy val bytes: ByteVector = tpe.bytes ++ payload
}

object LnMessage extends Factory[LnMessage[TLV]] {

  override def fromBytes(bytes: ByteVector): LnMessage[TLV] = {
    val tpe = BigSizeUInt(UInt16(bytes.take(2)).toInt)
    val value = bytes.drop(2)
    val length = BigSizeUInt(value.length)

    val tlv = TLV.fromBytes(tpe.bytes ++ length.bytes ++ value)
    LnMessage(tlv)
  }
}

case class LnMessageFactory[+T <: TLV](tlvFactory: TLVFactory[T])
    extends Factory[LnMessage[T]] {

  override def fromBytes(bytes: ByteVector): LnMessage[T] = {
    val tpe = BigSizeUInt(UInt16(bytes.take(2)).toInt)
    val value = bytes.drop(2)
    val length = BigSizeUInt(value.length)

    val tlv = tlvFactory.fromBytes(tpe.bytes ++ length.bytes ++ value)
    LnMessage(tlv)
  }
}
