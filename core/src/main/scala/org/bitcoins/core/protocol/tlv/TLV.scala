package org.bitcoins.core.protocol.tlv

import java.nio.charset.StandardCharsets

import org.bitcoins.core.number._
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.tlv.TLV.{
  DecodeTLVResult,
  FALSE_BYTE,
  TRUE_BYTE
}
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

sealed trait TLVParentFactory[T <: TLV] extends Factory[T] {

  def typeName: String

  def allFactories: Vector[TLVFactory[T]]

  lazy val knownTypes: Vector[BigSizeUInt] = allFactories.map(_.tpe)

  override def fromBytes(bytes: ByteVector): T = {
    val DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)

    allFactories.find(_.tpe == tpe) match {
      case Some(tlvFactory) => tlvFactory.fromTLVValue(value)
      case None =>
        throw new IllegalArgumentException(s"Unknown $typeName type got $tpe")
    }
  }
}

object TLV extends TLVParentFactory[TLV] {

  val FALSE_BYTE: Byte = 0x00
  val TRUE_BYTE: Byte = 0x01

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

  val typeName = "TLV"

  val allFactories: Vector[TLVFactory[TLV]] =
    Vector(ErrorTLV,
           PingTLV,
           PongTLV,
           OracleEventV0TLV,
           OracleAnnouncementV0TLV) ++ EventDescriptorTLV.allFactories

  // Need to override to be able to default to Unknown
  override def fromBytes(bytes: ByteVector): TLV = {
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

    def takeBoolean(): Boolean = {
      take(1).head match {
        case FALSE_BYTE => false
        case TRUE_BYTE  => true
        case byte: Byte =>
          throw new RuntimeException(
            s"Boolean values must be 0x00 or 0x01, got $byte")
      }
    }

    def takeString(): String = {
      val size = BigSizeUInt(current)
      skip(size.byteSize)
      val strBytes = take(size.toInt)
      new String(strBytes.toArray, StandardCharsets.UTF_8)
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

sealed trait EventDescriptorTLV extends TLV {
  def noncesNeeded: Int
}

object EventDescriptorTLV extends TLVParentFactory[EventDescriptorTLV] {

  val allFactories: Vector[TLVFactory[EventDescriptorTLV]] =
    Vector(EnumEventDescriptorV0TLV,
           RangeEventDescriptorV0TLV,
           DigitDecompositionEventDescriptorV0TLV)

  override def typeName: String = "EventDescriptorTLV"
}

/**
  * Describes an event over an enumerated set of outcomes
  * @param outcomes The set of possible outcomes
  * @see https://github.com/discreetlogcontracts/dlcspecs/blob/master/Oracle.md#simple-enumeration
  */
case class EnumEventDescriptorV0TLV(outcomes: Vector[String])
    extends EventDescriptorTLV {
  override def tpe: BigSizeUInt = EnumEventDescriptorV0TLV.tpe

  override val value: ByteVector = {
    val starting = UInt16(outcomes.size).bytes

    outcomes.foldLeft(starting) { (accum, outcome) =>
      val outcomeBytes = CryptoUtil.serializeForHash(outcome)
      accum ++ UInt16(outcomeBytes.length).bytes ++ outcomeBytes
    }
  }

  override def noncesNeeded: Int = 1
}

object EnumEventDescriptorV0TLV extends TLVFactory[EnumEventDescriptorV0TLV] {

  override val tpe: BigSizeUInt = BigSizeUInt(55302)

  override def fromTLVValue(value: ByteVector): EnumEventDescriptorV0TLV = {
    val iter = ValueIterator(value)

    val count = UInt16(iter.takeBits(16))

    val builder = Vector.newBuilder[String]

    while (iter.index < value.length) {
      val len = UInt16(iter.takeBits(16))
      val outcomeBytes = iter.take(len.toInt)
      val str = new String(outcomeBytes.toArray, StandardCharsets.UTF_8)
      builder.+=(str)
    }

    val result = builder.result()

    require(count.toInt == result.size,
            "Did not parse the expected number of outcomes")

    EnumEventDescriptorV0TLV(result)
  }
}

sealed trait NumericEventDescriptorTLV extends EventDescriptorTLV {

  /** The minimum valid value in the oracle can sign */
  def min: Vector[String]

  def minNum: BigInt

  /** The maximum valid value in the oracle can sign */
  def max: Vector[String]

  def maxNum: BigInt

  def step: UInt16

  def contains(outcome: BigInt): Boolean = {
    val inBounds = outcome <= maxNum && outcome >= minNum

    inBounds && (outcome - minNum) % step.toInt == 0
  }

  /** The base in which the outcome value is represented */
  def base: UInt16

  /** The unit of the outcome value */
  def unit: String

  /** The precision of the outcome representing the base exponent
    * by which to multiply the number represented by the composition
    * of the digits to obtain the actual outcome value.
    *
    * Modifies unit.
    */
  def precision: Int32

  lazy val precisionModifier: Double = Math.pow(base.toInt, precision.toInt)

  def stepToPrecision: BigDecimal = precisionModifier * BigDecimal(step.toInt)

  def minToPrecision: BigDecimal = precisionModifier * BigDecimal(minNum)

  def maxToPrecision: BigDecimal = precisionModifier * BigDecimal(maxNum)

  /** Checks if a outcome is contained in the set of outcomes when adjusted for precision
    * If you have precision=-1 and oracle outcomes [0,1,2,3...,10]
    * This would return true if passed a value [0, 0.1, 0.2,...,1.0]
    * If passed in the not precision adjusted outcomes [0,1,2,...10] it will return false
    */
  def containsPreciseOutcome(outcome: BigDecimal): Boolean = {
    (outcome / precisionModifier).toBigIntExact match {
      case Some(unModifiedOutcome) => contains(unModifiedOutcome)
      case None                    => false
    }
  }
}

/**
  * Describes a simple event over a range of numbers
  * @param start The first number in the range
  * @param count The number of possible outcomes
  * @param step The increment between each outcome
  */
case class RangeEventDescriptorV0TLV(
    start: Int32,
    count: UInt32,
    step: UInt16,
    unit: String,
    precision: Int32)
    extends NumericEventDescriptorTLV {

  override val minNum: BigInt = BigInt(start.toInt)

  override val min: Vector[String] = Vector(minNum.toString)

  override val maxNum: BigInt =
    start.toLong + (step.toLong * (count.toLong - 1))

  override val max: Vector[String] = Vector(maxNum.toString)

  override val base: UInt16 = UInt16(10)

  override val tpe: BigSizeUInt = RangeEventDescriptorV0TLV.tpe

  override val value: ByteVector = {
    val unitSize = BigSizeUInt(unit.length)
    val unitBytes = CryptoUtil.serializeForHash(unit)

    start.bytes ++ count.bytes ++ step.bytes ++
      unitSize.bytes ++ unitBytes ++ precision.bytes
  }

  override def noncesNeeded: Int = 1
}

object RangeEventDescriptorV0TLV extends TLVFactory[RangeEventDescriptorV0TLV] {

  override val tpe: BigSizeUInt = BigSizeUInt(55304)

  override def fromTLVValue(value: ByteVector): RangeEventDescriptorV0TLV = {
    val iter = ValueIterator(value)

    val start = Int32(iter.takeBits(32))
    val count = UInt32(iter.takeBits(32))
    val step = UInt16(iter.takeBits(16))

    val unit = iter.takeString()
    val precision = Int32(iter.takeBits(32))

    RangeEventDescriptorV0TLV(start, count, step, unit, precision)
  }
}

/** Describes a large range event using numerical decomposition */
trait DigitDecompositionEventDescriptorV0TLV extends NumericEventDescriptorTLV {
  require(numDigits > UInt16.zero,
          s"Number of digits must be positive, got $numDigits")

  /** Whether the outcome can be negative */
  def isSigned: Boolean

  /** The number of digits that the oracle will sign */
  def numDigits: UInt16

  override lazy val maxNum: BigInt = base.toBigInt.pow(numDigits.toInt) - 1

  private lazy val maxDigit = (base.toInt - 1).toString

  override lazy val max: Vector[String] = if (isSigned) {
    "+" +: Vector.fill(numDigits.toInt)(maxDigit)
  } else {
    Vector.fill(numDigits.toInt)(maxDigit)
  }

  override lazy val minNum: BigInt = if (isSigned) {
    -maxNum
  } else {
    0
  }

  override lazy val min: Vector[String] = if (isSigned) {
    "-" +: Vector.fill(numDigits.toInt)(maxDigit)
  } else {
    Vector.fill(numDigits.toInt)("0")
  }

  override lazy val step: UInt16 = UInt16.one

  override lazy val tpe: BigSizeUInt =
    DigitDecompositionEventDescriptorV0TLV.tpe

  override lazy val value: ByteVector = {
    val isSignedByte =
      if (isSigned) ByteVector(TRUE_BYTE) else ByteVector(FALSE_BYTE)

    val numDigitBytes = numDigits.bytes
    val unitSize = BigSizeUInt(unit.length)
    val unitBytes = CryptoUtil.serializeForHash(unit)

    base.bytes ++ isSignedByte ++ unitSize.bytes ++ unitBytes ++ precision.bytes ++ numDigitBytes
  }

  override def noncesNeeded: Int = {
    if (isSigned) numDigits.toInt + 1
    else numDigits.toInt
  }
}

/** Represents a large range event that can be positive or negative */
case class SignedDigitDecompositionEventDescriptor(
    base: UInt16,
    numDigits: UInt16,
    unit: String,
    precision: Int32)
    extends DigitDecompositionEventDescriptorV0TLV {
  override val isSigned: Boolean = true
}

/** Represents a large range event that is unsigned */
case class UnsignedDigitDecompositionEventDescriptor(
    base: UInt16,
    numDigits: UInt16,
    unit: String,
    precision: Int32)
    extends DigitDecompositionEventDescriptorV0TLV {
  override val isSigned: Boolean = false
}

object DigitDecompositionEventDescriptorV0TLV
    extends TLVFactory[DigitDecompositionEventDescriptorV0TLV] {

  override val tpe: BigSizeUInt = BigSizeUInt(55306)

  override def fromTLVValue(
      value: ByteVector): DigitDecompositionEventDescriptorV0TLV = {
    val iter = ValueIterator(value)

    val base = UInt16(iter.takeBits(16))
    val isSigned = iter.takeBoolean()

    val unit = iter.takeString()
    val precision = Int32(iter.takeBits(32))
    val numDigits = UInt16(iter.takeBits(16))

    DigitDecompositionEventDescriptorV0TLV(base,
                                           isSigned,
                                           numDigits.toInt,
                                           unit,
                                           precision)
  }

  def apply(
      base: UInt16,
      isSigned: Boolean,
      numDigits: Int,
      unit: String,
      precision: Int32): DigitDecompositionEventDescriptorV0TLV = {
    if (isSigned) {
      SignedDigitDecompositionEventDescriptor(base,
                                              UInt16(numDigits),
                                              unit,
                                              precision)
    } else {
      UnsignedDigitDecompositionEventDescriptor(base,
                                                UInt16(numDigits),
                                                unit,
                                                precision)
    }
  }
}

sealed trait OracleEventTLV extends TLV {
  def eventDescriptor: EventDescriptorTLV
  def nonces: Vector[SchnorrNonce]
}

case class OracleEventV0TLV(
    nonces: Vector[SchnorrNonce],
    eventMaturityEpoch: UInt32,
    eventDescriptor: EventDescriptorTLV,
    eventURI: String
) extends OracleEventTLV {

  require(eventDescriptor.noncesNeeded == nonces.size,
          "Not enough nonces for this event descriptor")

  override def tpe: BigSizeUInt = OracleEventV0TLV.tpe

  override val value: ByteVector = {
    val uriBytes = CryptoUtil.serializeForHash(eventURI)
    val numNonces = UInt16(nonces.size)
    val noncesBytes = nonces.foldLeft(numNonces.bytes)(_ ++ _.bytes)

    noncesBytes ++ eventMaturityEpoch.bytes ++ eventDescriptor.bytes ++ uriBytes
  }
}

object OracleEventV0TLV extends TLVFactory[OracleEventV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(55330)

  override def fromTLVValue(value: ByteVector): OracleEventV0TLV = {
    val iter = ValueIterator(value)

    val numNonces = UInt16(iter.takeBits(16))
    val builder = Vector.newBuilder[SchnorrNonce]

    for (_ <- 0 until numNonces.toInt) {
      val nonceBytes = iter.take(32)
      builder.+=(SchnorrNonce(nonceBytes))
    }

    val nonces = builder.result()

    require(
      numNonces.toInt == nonces.size,
      s"Did not parse the expected number of nonces expected ${numNonces.toInt}, got ${nonces.size}")

    val eventMaturity = UInt32(iter.takeBits(32))
    val eventDescriptor = EventDescriptorTLV(iter.current)
    iter.skip(eventDescriptor.byteSize)
    val eventURI = new String(iter.current.toArray, StandardCharsets.UTF_8)

    OracleEventV0TLV(nonces, eventMaturity, eventDescriptor, eventURI)
  }
}

sealed trait OracleAnnouncementTLV extends TLV {
  def eventTLV: OracleEventTLV
  def announcementSignature: SchnorrDigitalSignature
  def publicKey: SchnorrPublicKey
}

case class OracleAnnouncementV0TLV(
    announcementSignature: SchnorrDigitalSignature,
    publicKey: SchnorrPublicKey,
    eventTLV: OracleEventV0TLV)
    extends OracleAnnouncementTLV {
  override def tpe: BigSizeUInt = OracleAnnouncementV0TLV.tpe

  override val value: ByteVector =
    announcementSignature.bytes ++ publicKey.bytes ++ eventTLV.bytes
}

object OracleAnnouncementV0TLV extends TLVFactory[OracleAnnouncementV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(55332)

  override def fromTLVValue(value: ByteVector): OracleAnnouncementV0TLV = {
    val iter = ValueIterator(value)

    val sig = SchnorrDigitalSignature(iter.take(64))
    val publicKey = SchnorrPublicKey(iter.take(32))
    val eventTLV = OracleEventV0TLV(iter.current)

    OracleAnnouncementV0TLV(sig, publicKey, eventTLV)
  }
}
