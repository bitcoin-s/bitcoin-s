package org.bitcoins.core.protocol.tlv

import org.bitcoins.core.number.UInt16
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

/** Lightning Network message serialization is the same as TLV
  * serialization except that the type is represented with a
  * UInt16 instead of BigSizeUInt and length is omitted.
  *
  * The reason for the omission is that the message is expected to
  * be encrypted by the LN transport layer and the length is included
  * there in the unencrypted part of the packet.
  *
  * The reason that LnMessage doesn't just do what TLV does (which is better)
  * is because TLVs are newer and so we're stuck with the legacy format.
  */
case class LnMessage[+T <: TLV](tlv: T) extends NetworkElement {
  require(tlv.tpe.toLong <= 65535L, s"LN Message format requires UInt16 types")
  val tpe: UInt16 = UInt16(tlv.tpe.toInt)
  val payload: ByteVector = tlv.value
  override lazy val bytes: ByteVector = tpe.bytes ++ payload
  val typeName: String = tlv.typeName
}

object LnMessage extends Factory[LnMessage[TLV]] {

  override def fromBytes(bytes: ByteVector): LnMessage[TLV] = {
    val tpe = BigSizeUInt(UInt16(bytes.take(2)).toInt)
    val value = bytes.drop(2)
    val length = BigSizeUInt(value.length)

    val tlv = TLV.fromBytes(tpe.bytes ++ length.bytes ++ value)
    LnMessage(tlv)
  }

  def parseKnownMessage(bytes: ByteVector): LnMessage[TLV] = {
    val msg = fromBytes(bytes)

    msg.tlv match {
      case unknown: UnknownTLV =>
        throw new IllegalArgumentException(s"Parsed unknown TLV $unknown")
      case _: DLCSetupTLV | _: DLCSetupPieceTLV | _: InitTLV | _: DLCOracleTLV |
          _: ErrorTLV | _: PingTLV | _: PongTLV =>
        ()
    }

    msg
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
