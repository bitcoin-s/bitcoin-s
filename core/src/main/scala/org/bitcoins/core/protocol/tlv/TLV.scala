package org.bitcoins.core.protocol.tlv

import java.nio.charset.StandardCharsets

import org.bitcoins.core.number.{UInt16, UInt32}
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.tlv.TLV.DecodeTLVResult
import org.bitcoins.crypto._
import scodec.bits.ByteVector

sealed trait TLV extends NetworkElement {
  def tpe: BigSizeUInt
  def value: ByteVector

  def length: BigSizeUInt = {
    BigSizeUInt.calcFor(value)
  }

  override def bytes: ByteVector = {
    tpe.bytes ++ length.bytes ++ value
  }
}

object TLV extends Factory[TLV] {

  case class DecodeTLVResult(
      tpe: BigSizeUInt,
      length: BigSizeUInt,
      value: ByteVector)

  def decodeTLV(bytes: ByteVector): DecodeTLVResult = {
    val tpe = BigSizeUInt(bytes)
    val length = BigSizeUInt(bytes.drop(tpe.byteSize))
    val prefixSize = tpe.byteSize + length.byteSize

    require(
      bytes.length >= prefixSize + length.num.toLong,
      s"Length specified was $length but not enough bytes in ${bytes.drop(prefixSize)}")

    val value = bytes.drop(prefixSize).take(length.num.toLong)

    DecodeTLVResult(tpe, length, value)
  }

  private val allFactories: Vector[TLVFactory[TLV]] =
    Vector(ErrorTLV,
           PingTLV,
           PongTLV,
           OracleEventV0TLV,
           OracleAnnouncementV0TLV) ++ EventDescriptorTLV.allFactories

  val knownTypes: Vector[BigSizeUInt] = allFactories.map(_.tpe)

  def fromBytes(bytes: ByteVector): TLV = {
    val DecodeTLVResult(tpe, _, value) = decodeTLV(bytes)

    allFactories.find(_.tpe == tpe) match {
      case Some(tlvFactory) => tlvFactory.fromTLVValue(value)
      case None             => UnknownTLV(tpe, value)
    }
  }
}

sealed trait TLVFactory[+T <: TLV] extends Factory[T] {
  def tpe: BigSizeUInt
  def fromTLVValue(value: ByteVector): T

  override def fromBytes(bytes: ByteVector): T = {
    val DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)

    require(tpe == this.tpe, s"Invalid type $tpe when expecting ${this.tpe}")

    fromTLVValue(value)
  }

  protected case class ValueIterator(value: ByteVector, var index: Int = 0) {

    def current: ByteVector = {
      value.drop(index)
    }

    def skip(numBytes: Long): Unit = {
      index += numBytes.toInt
      ()
    }

    def skip(bytes: NetworkElement): Unit = {
      skip(bytes.byteSize)
    }

    def take(numBytes: Int): ByteVector = {
      val bytes = current.take(numBytes)
      skip(numBytes)
      bytes
    }

    def takeBits(numBits: Int): ByteVector = {
      require(numBits % 8 == 0,
              s"Must take a round byte number of bits, got $numBits")
      take(numBytes = numBits / 8)
    }

    def takeSPK(): ScriptPubKey = {
      val len = UInt16(takeBits(16)).toInt
      ScriptPubKey.fromAsmBytes(take(len))
    }
  }
}

case class UnknownTLV(tpe: BigSizeUInt, value: ByteVector) extends TLV {
  require(!TLV.knownTypes.contains(tpe), s"Type $tpe is known")
}

object UnknownTLV extends Factory[UnknownTLV] {

  override def fromBytes(bytes: ByteVector): UnknownTLV = {
    val DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)

    UnknownTLV(tpe, value)
  }
}

/** @see [[https://github.com/lightningnetwork/lightning-rfc/blob/master/01-messaging.md#the-error-message]] */
case class ErrorTLV(id: ByteVector, data: ByteVector) extends TLV {
  require(id.length == 32, s"ID associated with error is incorrect length: $id")

  override val tpe: BigSizeUInt = ErrorTLV.tpe

  override val value: ByteVector = {
    id ++ UInt16(data.length).bytes ++ data
  }
}

object ErrorTLV extends TLVFactory[ErrorTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(17)

  override def fromTLVValue(value: ByteVector): ErrorTLV = {
    val id = value.take(32)
    val len = UInt16(value.drop(32).take(2))
    val data = value.drop(32 + 2).take(len.toInt)

    ErrorTLV(id, data)
  }
}

case class PingTLV(numPongBytes: UInt16, ignored: ByteVector) extends TLV {
  override val tpe: BigSizeUInt = PingTLV.tpe

  override val value: ByteVector = {
    numPongBytes.bytes ++ UInt16(ignored.length).bytes ++ ignored
  }
}

object PingTLV extends TLVFactory[PingTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(18)

  override def fromTLVValue(value: ByteVector): PingTLV = {
    val numPongBytes = UInt16(value.take(2))
    val numIgnored = UInt16(value.slice(2, 4))
    val ignored = value.drop(4).take(numIgnored.toLong)

    PingTLV(numPongBytes, ignored)
  }
}

case class PongTLV(ignored: ByteVector) extends TLV {
  override val tpe: BigSizeUInt = PongTLV.tpe

  override val value: ByteVector = {
    UInt16(ignored.length).bytes ++ ignored
  }
}

object PongTLV extends TLVFactory[PongTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(19)

  override def fromTLVValue(value: ByteVector): PongTLV = {
    val numIgnored = UInt16(value.take(2))
    val ignored = value.drop(2).take(numIgnored.toLong)

    PongTLV.forIgnored(ignored)
  }

  def forIgnored(ignored: ByteVector): PongTLV = {
    new PongTLV(ignored)
  }
}

sealed trait EventDescriptorTLV extends TLV

object EventDescriptorTLV extends Factory[EventDescriptorTLV] {

  val allFactories: Vector[TLVFactory[EventDescriptorTLV]] =
    Vector(BasicEventDescriptorTLV)

  val knownTypes: Vector[BigSizeUInt] = allFactories.map(_.tpe)

  def fromBytes(bytes: ByteVector): EventDescriptorTLV = {
    val DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)

    allFactories.find(_.tpe == tpe) match {
      case Some(tlvFactory) => tlvFactory.fromTLVValue(value)
      case None =>
        throw new IllegalArgumentException(
          s"Unknown EventDescriptorTLV type got $tpe")
    }
  }
}

case class BasicEventDescriptorTLV(string: String) extends EventDescriptorTLV {
  override def tpe: BigSizeUInt = BasicEventDescriptorTLV.tpe

  override val value: ByteVector = CryptoUtil.serializeForHash(string)
}

object BasicEventDescriptorTLV extends TLVFactory[BasicEventDescriptorTLV] {

  override def apply(str: String): BasicEventDescriptorTLV =
    new BasicEventDescriptorTLV(str)

  override val tpe: BigSizeUInt = BigSizeUInt(55300)

  override def fromTLVValue(value: ByteVector): BasicEventDescriptorTLV = {
    val str = new String(value.toArray, StandardCharsets.UTF_8)

    BasicEventDescriptorTLV(str)
  }
}

sealed trait OracleEventTLV extends TLV

case class OracleEventV0TLV(
    publicKey: SchnorrPublicKey,
    nonce: SchnorrNonce,
    eventMaturity: UInt32,
    eventDescriptor: EventDescriptorTLV,
    eventURI: String
) extends OracleEventTLV {
  override def tpe: BigSizeUInt = OracleEventV0TLV.tpe

  override val value: ByteVector = {
    val uriBytes = CryptoUtil.serializeForHash(eventURI)
    publicKey.bytes ++ nonce.bytes ++ eventMaturity.bytes ++ eventDescriptor.bytes ++ uriBytes
  }
}

object OracleEventV0TLV extends TLVFactory[OracleEventV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(55330)

  override def fromTLVValue(value: ByteVector): OracleEventV0TLV = {
    val iter = ValueIterator(value, 0)

    val publicKey = SchnorrPublicKey(iter.take(32))
    val nonce = SchnorrNonce(iter.take(32))
    val eventMaturity = UInt32(iter.takeBits(32))
    val eventDescriptor = EventDescriptorTLV(iter.current)
    iter.skip(eventDescriptor.byteSize)
    val eventURI = new String(iter.current.toArray, StandardCharsets.UTF_8)

    OracleEventV0TLV(publicKey, nonce, eventMaturity, eventDescriptor, eventURI)
  }
}

sealed trait OracleAnnouncementTLV extends TLV

case class OracleAnnouncementV0TLV(
    signature: SchnorrDigitalSignature,
    eventTLV: OracleEventV0TLV)
    extends OracleAnnouncementTLV {
  override def tpe: BigSizeUInt = OracleAnnouncementV0TLV.tpe

  override val value: ByteVector = signature.bytes ++ eventTLV.bytes
}

object OracleAnnouncementV0TLV extends TLVFactory[OracleAnnouncementV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(55332)

  override def fromTLVValue(value: ByteVector): OracleAnnouncementV0TLV = {
    val iter = ValueIterator(value, 0)

    val sig = SchnorrDigitalSignature(iter.take(64))
    val eventTLV = OracleEventV0TLV(iter.current)

    OracleAnnouncementV0TLV(sig, eventTLV)
  }
}
