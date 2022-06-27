package org.bitcoins.core.protocol.tlv

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.dlc.compute.SigningVersion.DLCOracleV0SigningVersion
import org.bitcoins.core.protocol.dlc.models.{
  DLCPayoutCurve,
  OutcomePayoutPoint,
  PiecewisePolynomialEndpoint
}
import org.bitcoins.core.protocol.ln.PaymentSecret
import org.bitcoins.core.protocol.ln.channel.ShortChannelId
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv.OracleInfoV1TLV.oldTpe
import org.bitcoins.core.protocol.tlv.TLV.{
  DecodeTLVResult,
  FALSE_BYTE,
  TRUE_BYTE
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BigSizeUInt, BlockTimeStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.sorted.{OrderedAnnouncements, OrderedNonces}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import java.nio.charset.StandardCharsets
import java.time.Instant
import scala.annotation.tailrec
import scala.math.BigDecimal.RoundingMode
import scala.util.{Failure, Success, Try}

sealed trait DLCSpecType extends NetworkElement

sealed trait DLCPlainType extends DLCSpecType

sealed trait DLCSubType extends DLCSpecType {
  def subType: Byte
}

sealed trait TLV extends NetworkElement with TLVUtil {
  def tpe: BigSizeUInt
  def value: ByteVector

  def length: BigSizeUInt = {
    BigSizeUInt.calcFor(value)
  }

  override def bytes: ByteVector = {
    tpe.bytes ++ length.bytes ++ value
  }

  def sha256: Sha256Digest = CryptoUtil.sha256(bytes)

  def typeName: String = TLV.getTypeName(tpe)
}

trait TLVUtil {

  def boolBytes(bool: Boolean): ByteVector = {
    if (bool) {
      ByteVector(TRUE_BYTE)
    } else {
      ByteVector(FALSE_BYTE)
    }
  }

  def strBytes(str: NormalizedString): ByteVector = {
    TLV.getStringBytes(str)
  }

  def satBytes(sats: Satoshis): ByteVector = {
    UInt64(sats.toLong).bytes
  }

  def u16Prefix(bytes: ByteVector): ByteVector = {
    UInt16(bytes.length).bytes ++ bytes
  }

  def u16PrefixedList[T](
      vec: Vector[T],
      serialize: T => ByteVector): ByteVector = {
    vec.foldLeft(UInt16(vec.length).bytes) { case (accum, elem) =>
      accum ++ serialize(elem)
    }
  }

  def u16PrefixedList[T <: NetworkElement](vec: Vector[T]): ByteVector = {
    u16PrefixedList[T](vec, { elem: NetworkElement => elem.bytes })
  }

  def bigSizePrefix(bytes: ByteVector): ByteVector = {
    BigSizeUInt(bytes.length).bytes ++ bytes
  }

  def bigSizePrefixedList[T](
      vec: Vector[T],
      serialize: T => ByteVector): ByteVector = {
    vec.foldLeft(BigSizeUInt(vec.length).bytes) { case (accum, elem) =>
      accum ++ serialize(elem)
    }
  }

  def bigSizePrefixedList[T <: NetworkElement](vec: Vector[T]): ByteVector = {
    bigSizePrefixedList[T](vec, { elem: NetworkElement => elem.bytes })
  }
}

object TLVUtil extends TLVUtil

trait TLVSerializable[+T <: TLV] extends NetworkElement {
  def toTLV: T

  override def bytes: ByteVector = toTLV.bytes
}

trait DLCSpecTypeSerializable[+T <: DLCSpecType] extends NetworkElement {
  def toSubType: T

  override def bytes: ByteVector = toSubType.bytes
}

abstract class TLVDeserializable[T <: TLV, +U <: TLVSerializable[T]](
    tlvFactory: Factory[T])
    extends Factory[U] {
  def fromTLV(tlv: T): U

  override def fromBytes(bytes: ByteVector): U =
    fromTLV(tlvFactory.fromBytes(bytes))
}

abstract class DLCSpecTypeDeserializable[
    T <: DLCSpecType,
    +U <: DLCSpecTypeSerializable[T]](subTypeFactory: Factory[T])
    extends Factory[U] {
  def fromSubType(tlv: T): U

  override def fromBytes(bytes: ByteVector): U = {
    fromSubType(subTypeFactory.fromBytes(bytes))
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
        throw new IllegalArgumentException(
          s"Unknown $typeName type got $tpe (${TLV.getTypeName(tpe)})")
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

  override lazy val allFactories: Vector[TLVFactory[TLV]] = {
    Vector(
      InitTLV,
      ErrorTLV,
      AmtToForwardTLV,
      OutgoingCLTVValueTLV,
      ShortChannelIdTLV,
      PaymentDataTLV,
      PingTLV,
      PongTLV,
      OracleEventV0TLV,
//      RoundingIntervalsV0TLV,
//      PayoutFunctionV0TLV,
//      OracleParamsV0TLV,
      //ContractInfoV0TLV,
      //FundingInputV0TLV,
      //CETSignaturesV0TLV,
//      FundingSignaturesV0TLV,
      DLCOfferTLV,
      DLCAcceptTLV,
      DLCSignTLV,
      SendOfferTLV
    ) ++ EventDescriptorTLV.allFactories ++
      //PayoutCurvePieceTLV.allFactories ++
      //ContractDescriptorTLV.allFactories ++
      //ContractDescriptorTLV.allFactories ++
      //OracleInfoTLV.allFactories ++
      OracleAnnouncementTLV.allFactories ++
      OracleAttestmentTLV.allFactories //++
//      NegotiationFieldsTLV.allFactories
  }

  def getTypeName(tpe: BigSizeUInt): String = {
    allFactories
      .find(_.tpe == tpe)
      .map(_.typeName)
      .getOrElse("Unknown TLV type")
  }

  // Need to override to be able to default to Unknown
  override def fromBytes(bytes: ByteVector): TLV = {
    val DecodeTLVResult(tpe, _, value) = decodeTLV(bytes)

    allFactories.find(_.tpe == tpe) match {
      case Some(tlvFactory) => tlvFactory.fromTLVValue(value)
      case None             => UnknownTLV(tpe, value)
    }
  }

  def fromTypeAndValue(tpe: BigSizeUInt, value: ByteVector): TLV = {
    val bytes = tpe.bytes ++ BigSizeUInt.calcFor(value).bytes ++ value
    fromBytes(bytes)
  }

  def getStringBytes(str: NormalizedString): ByteVector = {
    val strBytes = str.bytes
    val size = BigSizeUInt(strBytes.size)

    size.bytes ++ strBytes
  }

  def encodeScript(script: Script): ByteVector = {
    UInt16(script.asmBytes.length).bytes ++ script.asmBytes
  }
}

sealed trait TLVFactory[+T <: TLV] extends Factory[T] {
  def tpe: BigSizeUInt

  def typeName: String

  def fromTLVValue(value: ByteVector): T

  override def fromBytes(bytes: ByteVector): T = {
    val DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)

    require(tpe == this.tpe,
            s"Got ${TLV.getTypeName(tpe)} type when expecting ${typeName}")

    fromTLVValue(value)
  }
}

sealed trait DLCSubTypeFactory[+T <: DLCSubType] extends Factory[T]

case class NormalizedString(private val str: String) extends NetworkElement {

  val normStr: String = CryptoUtil.normalize(str)

  override def equals(other: Any): Boolean = {
    other match {
      case otherStr: String =>
        normStr == otherStr
      case otherNorm: NormalizedString =>
        normStr == otherNorm.normStr
      case _ => other.equals(str)
    }
  }

  override def toString: String = normStr

  override def bytes: ByteVector = CryptoUtil.serializeForHash(normStr)
}

object NormalizedString extends StringFactory[NormalizedString] {

  def apply(bytes: ByteVector): NormalizedString = {
    NormalizedString(new String(bytes.toArray, StandardCharsets.UTF_8))
  }

  import scala.language.implicitConversions

  implicit def stringToNormalized(str: String): NormalizedString =
    NormalizedString(str)

  implicit def normalizedToString(normalized: NormalizedString): String =
    normalized.normStr

  // If other kinds of Iterables are needed, there's a fancy thing to do
  // that is done all over the Seq code using params and an implicit CanBuildFrom
  implicit def stringVecToNormalized(
      strs: Vector[String]): Vector[NormalizedString] =
    strs.map(apply)

  implicit def normalizedVecToString(
      strs: Vector[NormalizedString]): Vector[String] =
    strs.map(_.normStr)

  override def fromString(string: String): NormalizedString =
    NormalizedString(string)
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

case class AmtToForwardTLV(amt: MilliSatoshis) extends TLV {
  override val tpe: BigSizeUInt = AmtToForwardTLV.tpe

  override val value: ByteVector = {
    amt.toUInt64.truncatedBytes
  }
}

object AmtToForwardTLV extends TLVFactory[AmtToForwardTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(2)

  override def fromTLVValue(value: ByteVector): AmtToForwardTLV = {
    val uint64 = UInt64.fromTruncatedBytes(value)
    val msat = MilliSatoshis(uint64.toBigInt)

    AmtToForwardTLV(msat)
  }

  override val typeName: String = "AmtToForwardTLV"
}

case class OutgoingCLTVValueTLV(cltv: UInt32) extends TLV {
  override val tpe: BigSizeUInt = OutgoingCLTVValueTLV.tpe

  override val value: ByteVector = {
    cltv.truncatedBytes
  }
}

object OutgoingCLTVValueTLV extends TLVFactory[OutgoingCLTVValueTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(4)

  override def fromTLVValue(value: ByteVector): OutgoingCLTVValueTLV = {
    val iter = ValueIterator(value)

    val cltv = UInt32.fromTruncatedBytes(iter.current)

    OutgoingCLTVValueTLV(cltv)
  }

  override val typeName: String = "OutgoingCLTVValueTLV"
}

case class ShortChannelIdTLV(scid: ShortChannelId) extends TLV {
  override val tpe: BigSizeUInt = ShortChannelIdTLV.tpe

  override val value: ByteVector = scid.bytes
}

object ShortChannelIdTLV extends TLVFactory[ShortChannelIdTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(6)

  override def fromTLVValue(value: ByteVector): ShortChannelIdTLV = {
    val iter = ValueIterator(value)

    val scid = iter.take(ShortChannelId, 8)

    ShortChannelIdTLV(scid)
  }

  override val typeName: String = "ShortChannelIdTLV"
}

case class PaymentDataTLV(paymentSecret: PaymentSecret, msats: MilliSatoshis)
    extends TLV {
  override val tpe: BigSizeUInt = PaymentDataTLV.tpe

  override val value: ByteVector = {
    paymentSecret.bytes ++ msats.toUInt64.truncatedBytes
  }
}

object PaymentDataTLV extends TLVFactory[PaymentDataTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(8)

  override def fromTLVValue(value: ByteVector): PaymentDataTLV = {
    val iter = ValueIterator(value)

    val secret = iter.take(PaymentSecret, 32)
    val uint64 = UInt64.fromTruncatedBytes(iter.current)
    val msat = MilliSatoshis(uint64.toBigInt)

    PaymentDataTLV(secret, msat)
  }

  override val typeName: String = "PaymentDataTLV"
}

/** @see https://github.com/lightningnetwork/lightning-rfc/blob/master/01-messaging.md#the-init-message */
case class InitTLV(
    globalFeatureBytes: ByteVector,
    featureBytes: ByteVector,
    initTLVs: Vector[TLV])
    extends TLV {
  initTLVs.collect { case UnknownTLV(tpe, _) =>
    require(tpe.toBigInt % 2 != 0,
            s"Cannot have unknown even initTLVs, got $initTLVs")
  }

  require(initTLVs.map(_.tpe).distinct.size == initTLVs.size,
          s"Cannot have duplicate TLV types in initTLVs, got $initTLVs")

  override val tpe: BigSizeUInt = InitTLV.tpe

  override val value: ByteVector = {
    u16Prefix(globalFeatureBytes) ++ u16Prefix(featureBytes) ++
      initTLVs.foldLeft(ByteVector.empty)(_ ++ _.bytes)
  }
}

object InitTLV extends TLVFactory[InitTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(16)

  override def fromTLVValue(value: ByteVector): InitTLV = {
    val iter = ValueIterator(value)

    val global = iter.takeU16Prefixed(iter.take)
    val features = iter.takeU16Prefixed(iter.take)

    @tailrec
    def loop(accum: Vector[TLV]): Vector[TLV] = {
      if (iter.finished) accum
      else {
        val next = accum :+ iter.take(TLV)
        loop(next)
      }
    }

    val tlvs = loop(Vector.empty)

    InitTLV(global, features, tlvs)
  }

  override val typeName: String = "InitTLV"
}

/** @see [[https://github.com/lightningnetwork/lightning-rfc/blob/master/01-messaging.md#the-error-message]] */
case class ErrorTLV(id: ByteVector, data: ByteVector) extends TLV {
  require(id.length == 32, s"ID associated with error is incorrect length: $id")

  override val tpe: BigSizeUInt = ErrorTLV.tpe

  override val value: ByteVector = {
    id ++ u16Prefix(data)
  }
}

object ErrorTLV extends TLVFactory[ErrorTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(17)

  override def fromTLVValue(value: ByteVector): ErrorTLV = {
    val iter = ValueIterator(value)

    val id = iter.take(32)
    val data = iter.takeU16Prefixed(iter.take)

    ErrorTLV(id, data)
  }

  override val typeName: String = "ErrorTLV"
}

case class PingTLV(numPongBytes: UInt16, ignored: ByteVector) extends TLV {
  override val tpe: BigSizeUInt = PingTLV.tpe

  override val value: ByteVector = {
    numPongBytes.bytes ++ u16Prefix(ignored)
  }
}

object PingTLV extends TLVFactory[PingTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(18)

  override def fromTLVValue(value: ByteVector): PingTLV = {
    val iter = ValueIterator(value)

    val numPongBytes = iter.takeU16()
    val ignored = iter.takeU16Prefixed(iter.take)

    PingTLV(numPongBytes, ignored)
  }

  override val typeName: String = "PingTLV"
}

case class PongTLV(ignored: ByteVector) extends TLV {
  override val tpe: BigSizeUInt = PongTLV.tpe

  override val value: ByteVector = {
    u16Prefix(ignored)
  }
}

object PongTLV extends TLVFactory[PongTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(19)

  override def fromTLVValue(value: ByteVector): PongTLV = {
    val iter = ValueIterator(value)

    val ignored = iter.takeU16Prefixed(iter.take)

    PongTLV.forIgnored(ignored)
  }

  def forIgnored(ignored: ByteVector): PongTLV = {
    new PongTLV(ignored)
  }

  override val typeName: String = "PongTLV"
}

sealed trait DLCOracleTLV extends TLV

sealed trait EventDescriptorTLV extends DLCOracleTLV {
  def noncesNeeded: Int

  /** Event descriptors all use the same signing version as of now.
    * @see https://github.com/discreetlogcontracts/dlcspecs/pull/113
    */
  def signingVersion: SigningVersion = DLCOracleV0SigningVersion
}

object EventDescriptorTLV extends TLVParentFactory[EventDescriptorTLV] {

  val allFactories: Vector[TLVFactory[EventDescriptorTLV]] =
    Vector(EnumEventDescriptorV0TLV, DigitDecompositionEventDescriptorV0TLV)

  override val typeName: String = "EventDescriptorTLV"
}

/** Describes an event over an enumerated set of outcomes
  * @param outcomes The set of possible outcomes
  * @see https://github.com/discreetlogcontracts/dlcspecs/blob/master/Oracle.md#simple-enumeration
  */
case class EnumEventDescriptorV0TLV(outcomes: Vector[NormalizedString])
    extends EventDescriptorTLV {
  override def tpe: BigSizeUInt = EnumEventDescriptorV0TLV.tpe

  override val value: ByteVector = {
    u16PrefixedList(outcomes, TLV.getStringBytes)
  }

  override def noncesNeeded: Int = 1
}

object EnumEventDescriptorV0TLV extends TLVFactory[EnumEventDescriptorV0TLV] {

  override val tpe: BigSizeUInt = BigSizeUInt(55302) //fdd806

  override def fromTLVValue(value: ByteVector): EnumEventDescriptorV0TLV = {
    val iter = ValueIterator(value)

    val outcomes = iter.takeU16PrefixedList(() => iter.takeString())

    EnumEventDescriptorV0TLV(outcomes)
  }

  val dummy: EnumEventDescriptorV0TLV = EnumEventDescriptorV0TLV(
    Vector("dummy"))

  override val typeName: String = "EnumEventDescriptorV0TLV"
}

sealed trait NumericEventDescriptorTLV extends EventDescriptorTLV {

  /** The minimum valid value in the oracle can sign */
  def min: Vector[NormalizedString]

  def minNum: BigInt

  /** The maximum valid value in the oracle can sign */
  def max: Vector[NormalizedString]

  def maxNum: BigInt

  def step: UInt16

  def contains(outcome: BigInt): Boolean = {
    val inBounds = outcome <= maxNum && outcome >= minNum

    inBounds && (outcome - minNum) % step.toInt == 0
  }

  /** The base in which the outcome value is represented */
  def base: UInt16

  /** The unit of the outcome value */
  def unit: NormalizedString

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

  /** If the Descriptor contains negative values */
  def isSigned: Boolean
}

/** Describes a large range event using numerical decomposition */
sealed trait DigitDecompositionEventDescriptorV0TLV
    extends NumericEventDescriptorTLV {
  require(numDigits > UInt16.zero,
          s"Number of digits must be positive, got $numDigits")

  /** The number of digits that the oracle will sign */
  def numDigits: UInt16

  override lazy val maxNum: BigInt = base.toBigInt.pow(numDigits.toInt) - 1

  private lazy val maxDigit: NormalizedString = (base.toInt - 1).toString

  override lazy val max: Vector[NormalizedString] = {
    this match {
      case _: SignedDigitDecompositionEventDescriptor =>
        NormalizedString("+") +: Vector.fill(numDigits.toInt)(maxDigit)
      case _: UnsignedDigitDecompositionEventDescriptor =>
        Vector.fill(numDigits.toInt)(maxDigit)

    }
  }

  override lazy val minNum: BigInt = {
    this match {
      case _: SignedDigitDecompositionEventDescriptor =>
        -maxNum
      case _: UnsignedDigitDecompositionEventDescriptor =>
        0
    }
  }

  override lazy val min: Vector[NormalizedString] = {
    this match {
      case _: SignedDigitDecompositionEventDescriptor =>
        NormalizedString("-") +: Vector.fill(numDigits.toInt)(maxDigit)
      case _: UnsignedDigitDecompositionEventDescriptor =>
        Vector.fill(numDigits.toInt)("0")
    }
  }

  override lazy val step: UInt16 = UInt16.one

  override lazy val tpe: BigSizeUInt =
    DigitDecompositionEventDescriptorV0TLV.tpe

  override lazy val value: ByteVector = {
    val start = base.bytes
    val signByte = this match {
      case _: UnsignedDigitDecompositionEventDescriptor =>
        boolBytes(false)
      case _: SignedDigitDecompositionEventDescriptor =>
        boolBytes(true)
    }
    val end = strBytes(unit) ++
      precision.bytes ++
      numDigits.bytes
    start ++ signByte ++ end
  }

  override def noncesNeeded: Int = {
    this match {
      case _: SignedDigitDecompositionEventDescriptor =>
        numDigits.toInt + 1
      case _: UnsignedDigitDecompositionEventDescriptor =>
        numDigits.toInt
    }
  }
}

/** Represents a large range event that can be positive or negative */
case class SignedDigitDecompositionEventDescriptor(
    base: UInt16,
    numDigits: UInt16,
    unit: NormalizedString,
    precision: Int32)
    extends DigitDecompositionEventDescriptorV0TLV {

  override val isSigned: Boolean = true
}

/** Represents a large range event that is unsigned */
case class UnsignedDigitDecompositionEventDescriptor(
    base: UInt16,
    numDigits: UInt16,
    unit: NormalizedString,
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

    val base = iter.takeU16()
    val isSigned = iter.takeBoolean()
    val unit = iter.takeString()
    val precision = iter.takeI32()
    val numDigits = iter.takeU16()

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
      unit: NormalizedString,
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

  override val typeName: String = "DigitDecompositionEventDescriptorV0TLV"
}

sealed trait OracleEventTLV extends DLCOracleTLV {
  def eventDescriptor: EventDescriptorTLV
  def nonces: OrderedNonces
  def eventId: NormalizedString
  def eventMaturityEpoch: UInt32
}

case class OracleEventV0TLV(
    nonces: OrderedNonces,
    eventMaturityEpoch: UInt32,
    eventDescriptor: EventDescriptorTLV,
    eventId: NormalizedString
) extends OracleEventTLV {

  require(
    eventDescriptor.noncesNeeded == nonces.vec.size,
    s"Not enough nonces for this event descriptor, noncesNeeded=${eventDescriptor.noncesNeeded} nonces=${nonces.toVector.size}"
  )

  override def tpe: BigSizeUInt = OracleEventV0TLV.tpe

  override val value: ByteVector = {
    val b = u16PrefixedList(nonces.vec)
    b ++
      eventMaturityEpoch.bytes ++
      eventDescriptor.bytes ++
      strBytes(eventId)
  }

  /** Gets the maturation of the event since epoch */
  def maturation: Instant = {
    Instant.ofEpochSecond(eventMaturityEpoch.toLong)
  }
}

object OracleEventV0TLV extends TLVFactory[OracleEventV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(55330)

  override def fromTLVValue(value: ByteVector): OracleEventV0TLV = {
    val iter = ValueIterator(value)

    val nonces = iter.takeU16PrefixedList(() => iter.take(SchnorrNonce, 32))

    val eventMaturity = iter.takeU32()

    val eventDescriptor = iter.take(EventDescriptorTLV)
    val eventId = iter.takeString()

    OracleEventV0TLV(OrderedNonces(nonces),
                     eventMaturity,
                     eventDescriptor,
                     eventId)
  }

  override val typeName: String = "OracleEventV0TLV"
}

sealed trait OracleAnnouncementTLV extends DLCOracleTLV {
  def eventTLV: OracleEventTLV
  def announcementSignature: SchnorrDigitalSignature
  def publicKey: SchnorrPublicKey

  def validateSignature: Boolean
}

object OracleAnnouncementTLV extends TLVParentFactory[OracleAnnouncementTLV] {

  val allFactories: Vector[TLVFactory[OracleAnnouncementTLV]] =
    Vector(OracleAnnouncementV0TLV)

  override val typeName: String = "OracleAnnouncementTLV"
}

case class OracleAnnouncementV0TLV(
    announcementSignature: SchnorrDigitalSignature,
    publicKey: SchnorrPublicKey,
    eventTLV: OracleEventV0TLV)
    extends OracleAnnouncementTLV {
  override def tpe: BigSizeUInt = OracleAnnouncementV0TLV.tpe

  override val value: ByteVector = {
    announcementSignature.bytes ++ publicKey.bytes ++ eventTLV.bytes
  }

  override def bytes: ByteVector = {
    tpe.bytes ++ length.bytes ++ value
  }

  override def validateSignature: Boolean = {
    publicKey.verify(CryptoUtil
                       .sha256DLCAnnouncement(eventTLV.bytes)
                       .bytes,
                     announcementSignature)
  }
}

object OracleAnnouncementV0TLV extends TLVFactory[OracleAnnouncementV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(55332)

  override def fromTLVValue(value: ByteVector): OracleAnnouncementV0TLV = {
    val iter = ValueIterator(value)
    val sig = iter.take(SchnorrDigitalSignature, 64)
    val publicKey = iter.take(SchnorrPublicKey, 32)
    val eventTLV = iter.take(OracleEventV0TLV)

    OracleAnnouncementV0TLV(sig, publicKey, eventTLV)
  }

  lazy val dummy: OracleAnnouncementV0TLV = {
    val dummyPrivKey: ECPrivateKey = ECPrivateKey.fromHex(
      "f04671ab68f3fefbeaa344c49149748f722287a81b19cd956b2332d07b8f6853")
    val event = OracleEventV0TLV(
      OrderedNonces(Vector(dummyPrivKey.schnorrNonce)),
      UInt32.zero,
      EnumEventDescriptorV0TLV.dummy,
      "dummy")
    val sig =
      dummyPrivKey.schnorrSign(
        CryptoUtil.sha256DLCAnnouncement(event.bytes).bytes)

    OracleAnnouncementV0TLV(sig, dummyPrivKey.schnorrPublicKey, event)
  }

  def dummyForEventsAndKeys(
      privKey: ECPrivateKey,
      nonce: SchnorrNonce,
      events: Vector[EnumOutcome]): OracleAnnouncementTLV = {
    val event = OracleEventV0TLV(
      OrderedNonces(Vector(nonce)),
      UInt32.zero,
      EnumEventDescriptorV0TLV(events.map(outcome => outcome.outcome)),
      "dummy")
    val sig =
      privKey.schnorrSign(CryptoUtil.sha256DLCAnnouncement(event.bytes).bytes)

    OracleAnnouncementV0TLV(sig, privKey.schnorrPublicKey, event)
  }

  def dummyForKeys(
      privKey: ECPrivateKey,
      nonces: Vector[SchnorrNonce]): OracleAnnouncementTLV = {
    val eventDescriptor = DigitDecompositionEventDescriptorV0TLV(UInt16(2),
                                                                 isSigned =
                                                                   false,
                                                                 nonces.length,
                                                                 "dummy",
                                                                 Int32.zero)
    val event = OracleEventV0TLV(OrderedNonces(nonces),
                                 UInt32.zero,
                                 eventDescriptor,
                                 "dummy")
    val sig =
      privKey.schnorrSign(CryptoUtil.sha256DLCAnnouncement(event.bytes).bytes)

    OracleAnnouncementV0TLV(sig, privKey.schnorrPublicKey, event)
  }

  override val typeName: String = "OracleAnnouncementV0TLV"
}

sealed trait OracleAttestmentTLV extends DLCOracleTLV {
  def eventId: NormalizedString
  def publicKey: SchnorrPublicKey
  def sigs: Vector[SchnorrDigitalSignature]
  def outcomes: Vector[NormalizedString]
}

object OracleAttestmentTLV extends TLVParentFactory[OracleAttestmentTLV] {

  val allFactories: Vector[TLVFactory[OracleAttestmentTLV]] =
    Vector(OracleAttestmentV0TLV)

  override val typeName: String = "OracleAttestmentTLV"
}

case class OracleAttestmentV0TLV(
    eventId: NormalizedString,
    publicKey: SchnorrPublicKey,
    sigs: Vector[SchnorrDigitalSignature],
    outcomes: Vector[NormalizedString])
    extends OracleAttestmentTLV {
  require(sigs.nonEmpty, "Cannot have 0 signatures")
  require(
    outcomes.size == sigs.size,
    s"Number of outcomes must match number of signatures, ${outcomes.size} != ${sigs.size}")
  override val tpe: BigSizeUInt = OracleAttestmentV0TLV.tpe

  override val value: ByteVector = {
    val outcomesBytes = outcomes.foldLeft(ByteVector.empty) {
      case (accum, elem) =>
        accum ++ strBytes(elem)
    }

    strBytes(eventId) ++
      publicKey.bytes ++
      u16PrefixedList(sigs) ++
      outcomesBytes
  }
}

object OracleAttestmentV0TLV extends TLVFactory[OracleAttestmentV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(55400)

  override def fromTLVValue(value: ByteVector): OracleAttestmentV0TLV = {
    val iter = ValueIterator(value)

    val eventId = iter.takeString()
    val pubKey = iter.take(SchnorrPublicKey, 32)
    val sigs =
      iter.takeU16PrefixedList(() => iter.take(SchnorrDigitalSignature, 64))
    val outcomes = sigs.indices.toVector.map { _ =>
      iter.takeString()
    }

    OracleAttestmentV0TLV(eventId, pubKey, sigs, outcomes)
  }

  lazy val dummy: OracleAttestmentV0TLV = {
    val eventId = NormalizedString("dummy")
    val key = ECPrivateKey.freshPrivateKey
    val outcome = NormalizedString("outcome")
    val sig = key.schnorrSign(CryptoUtil.sha256DLCAttestation(outcome).bytes)

    OracleAttestmentV0TLV(eventId,
                          key.schnorrPublicKey,
                          Vector(sig),
                          Vector(outcome))
  }

  override val typeName: String = "OracleAttestmentV0TLV"
}

sealed trait DLCSetupPieceTLV extends TLV

sealed trait ContractDescriptorTLV extends DLCSubType

object ContractDescriptorTLV extends Factory[ContractDescriptorTLV] {

  val allFactories: Vector[Factory[ContractDescriptorTLV]] =
    Vector(ContractDescriptorV0TLV, ContractDescriptorV1TLV)

  val empty: ContractDescriptorTLV =
    ContractDescriptorV0TLV(Vector.empty, DLCSerializationVersion.current)

  private val knownSubTypes: Vector[Byte] = {
    Vector(0.toByte, 1.toByte)
  }

  private val subTypeMap: Map[Byte, Factory[ContractDescriptorTLV]] = {
    knownSubTypes.zip(allFactories).toMap
  }

  private def fromOldTLVValue(bytes: ByteVector): ContractDescriptorTLV = {
    Try(ContractDescriptorV0TLV.fromBytes(bytes))
      .getOrElse(ContractDescriptorV1TLV.fromBytes(bytes))
  }

  override def fromBytes(bytes: ByteVector): ContractDescriptorTLV = {
    val t = Try {
      SubTypeUtil.fromBytes(bytes, subTypeMap)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }
}

/** @see https://github.com/discreetlogcontracts/dlcspecs/blob/master/Messaging.md#version-0-contract_info */
case class ContractDescriptorV0TLV(
    outcomes: Vector[(String, Satoshis)],
    serializationVersion: DLCSerializationVersion)
    extends ContractDescriptorTLV {

  override val subType: Byte = ContractDescriptorV0TLV.subType

  override val bytes: ByteVector = {
    val outcomeBytes = TLVUtil.bigSizePrefixedList[(String, Satoshis)](
      outcomes,
      { case (outcome, amt) =>
        val outcomeBytes = CryptoUtil.serializeForHash(outcome)
        TLVUtil.bigSizePrefix(outcomeBytes) ++ TLVUtil.satBytes(amt)
      })

    ByteVector.fromByte(subType) ++ outcomeBytes
  }

  override val byteSize: Long = {
    serializationVersion match {
      case DLCSerializationVersion.Gamma => super.byteSize
      case DLCSerializationVersion.Alpha | DLCSerializationVersion.Beta =>
        val value = TLVUtil.bigSizePrefixedList[(String, Satoshis)](
          outcomes,
          { case (outcome, amt) =>
            val outcomeBytes = CryptoUtil.serializeForHash(outcome)
            TLVUtil.bigSizePrefix(outcomeBytes) ++ TLVUtil.satBytes(amt)
          })

        ContractDescriptorV0TLV.oldTpe.byteSize + BigSizeUInt
          .calcFor(value)
          .byteSize + value.size
    }
  }
}

object ContractDescriptorV0TLV extends Factory[ContractDescriptorV0TLV] {
  final val subType: Byte = 0.toByte

  private val oldTpe: BigSizeUInt = BigSizeUInt(42768)

  private def fromOldTLVValue(bytes: ByteVector): ContractDescriptorV0TLV = {
    val DecodeTLVResult(tpe, len, value) = TLV.decodeTLV(bytes)

    require(
      tpe == oldTpe,
      s"Invalid type $tpe (${TLV.getTypeName(tpe)}) len=$len when expecting ${this.oldTpe}")
    val iter = ValueIterator(value)
    val outcomes = iter.takeBigSizePrefixedList { () =>
      val outcome = iter.takeString().normStr
      val amt = iter.takeSats()

      outcome -> amt
    }
    //since beta wasn't in a released version we always
    //assume the old version is beta
    //TODO: Need to come back and support alpha again
    val descriptor =
      ContractDescriptorV0TLV(outcomes, DLCSerializationVersion.Beta)
    descriptor
  }

  override def fromBytes(bytes: ByteVector): ContractDescriptorV0TLV = {
    val t = Try.apply {
      require(
        bytes.head == subType,
        s"Required subtype is $subType for ContractDescriptorV0TLV, got=${bytes.head}")
      val iter = ValueIterator(bytes.drop(1))

      val outcomes = iter.takeBigSizePrefixedList { () =>
        val outcome = iter.takeString().normStr
        val amt = iter.takeSats()

        outcome -> amt
      }
      ContractDescriptorV0TLV(outcomes, DLCSerializationVersion.Gamma)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }
}

case class RoundingIntervalsV0TLV(
    intervalStarts: Vector[(Long, Satoshis)],
    serializationVersion: DLCSerializationVersion)
    extends DLCPlainType {
  def isEmpty: Boolean = intervalStarts.isEmpty

  val oldTpe: BigSizeUInt = RoundingIntervalsV0TLV.oldTpe

  override val bytes: ByteVector = {
    TLVUtil.bigSizePrefixedList[(Long, Satoshis)](
      intervalStarts,
      { case (outcome, roundingModSats) =>
        UInt64(outcome).bytes ++
          UInt64(roundingModSats.toLong).bytes
      })
  }

  override val byteSize: Long = {
    serializationVersion match {
      case DLCSerializationVersion.Gamma => super.byteSize
      case DLCSerializationVersion.Alpha | DLCSerializationVersion.Beta =>
        val value = {
          TLVUtil.u16PrefixedList[(Long, Satoshis)](
            intervalStarts,
            { case (outcome, roundingModSats) =>
              BigSizeUInt(outcome).bytes ++
                BigSizeUInt(roundingModSats.toLong).bytes
            })
        }
        RoundingIntervalsV0TLV.oldTpe.byteSize + BigSizeUInt
          .calcFor(value)
          .byteSize + value.size
    }
  }
}

object RoundingIntervalsV0TLV extends Factory[RoundingIntervalsV0TLV] {
  val oldTpe: BigSizeUInt = BigSizeUInt(42788)

  val noRounding: RoundingIntervalsV0TLV =
    RoundingIntervalsV0TLV(Vector.empty, DLCSerializationVersion.current)

  private def fromTLVValue(bytes: ByteVector): RoundingIntervalsV0TLV = {

    val TLV.DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)
    require(tpe == oldTpe,
            s"Incorrect tpe for RoundingIntervalsV0TLV, got=$tpe expected=$tpe")

    val iter = ValueIterator(value)

    val intervalStarts = iter.takeU16PrefixedList { () =>
      val outcome = iter.takeBigSize().toLong
      val roundingMod = Satoshis(iter.takeBigSize().toLong)

      (outcome, roundingMod)
    }

    RoundingIntervalsV0TLV(intervalStarts, DLCSerializationVersion.Beta)
  }

  override def fromBytes(bytes: ByteVector): RoundingIntervalsV0TLV = {
    val iter = ValueIterator(bytes)
    val roundingIntervalsT = Try {
      val intervalStarts = iter.takeBigSizePrefixedList { () =>
        val outcome = iter.takeU64().toLong
        val roundingMod = Satoshis(iter.takeU64())
        (outcome, roundingMod)
      }

      RoundingIntervalsV0TLV(intervalStarts, DLCSerializationVersion.Gamma)
    }
    roundingIntervalsT.getOrElse(fromTLVValue(bytes))
  }

  //override val typeName: String = "RoundingIntervalsV0TLV"
}

case class TLVPoint(
    outcome: Long,
    value: Satoshis,
    extraPrecision: Int,
    serializationVersion: DLCSerializationVersion)
    extends NetworkElement {

  lazy val bigDecimalPayout: BigDecimal = {
    value.toLong + (BigDecimal(extraPrecision) / (1 << 16))
  }

  override def bytes: ByteVector = {
    UInt64(outcome).bytes ++
      UInt64(value.toLong).bytes ++
      UInt16(extraPrecision).bytes
  }

  override val byteSize: Long = {
    serializationVersion match {
      case DLCSerializationVersion.Gamma => super.byteSize
      case DLCSerializationVersion.Alpha | DLCSerializationVersion.Beta =>
        val outcomeBigSize = BigSizeUInt(outcome)
        val valueBigSize = BigSizeUInt(value.toLong)
        val extraPrecision = UInt16(
          bytes.drop(outcomeBigSize.byteSize + valueBigSize.byteSize).take(2))
        outcomeBigSize.byteSize + valueBigSize.byteSize + extraPrecision.byteSize
    }
  }

  def outcomePayoutPoint: OutcomePayoutPoint = {
    OutcomePayoutPoint(outcome, value.toLong)
  }

  override def toString: String = {
    s"TLVPoint(outcome=$outcome,value=$value,extraPrecision=$extraPrecision)"
  }
}

object TLVPoint extends Factory[TLVPoint] {

  /** We broke TLVPoint serialization in this commit
    * @see https://github.com/discreetlogcontracts/dlcspecs/pull/163/commits/44afd6153e35cff61e48fed0f7fb047407dff09c
    */
  private def fromOldBytes(bytes: ByteVector): TLVPoint = {
    val outcome = BigSizeUInt(bytes)
    val value = BigSizeUInt(bytes.drop(outcome.byteSize))
    val extraPrecision = UInt16(
      bytes.drop(outcome.byteSize + value.byteSize).take(2)).toInt
    TLVPoint(outcome = outcome.toLong,
             value = Satoshis(value.toLong),
             extraPrecision = extraPrecision,
             DLCSerializationVersion.Beta)
  }

  override def fromBytes(bytes: ByteVector): TLVPoint = {
    val t = Try {
      val iter = ValueIterator(bytes)
      val outcome = iter.takeU64()
      val value = iter.takeU64()
      val extraPrecision = iter.takeU16()
      TLVPoint(outcome = outcome.toBigInt.toLong,
               value = Satoshis(value),
               extraPrecision = extraPrecision.toInt,
               DLCSerializationVersion.Gamma)
    }
    t.getOrElse(fromOldBytes(bytes))
  }
}

case class OldTLVPoint(
    outcome: Long,
    value: Satoshis,
    extraPrecision: Int,
    isEndpoint: Boolean)
    extends NetworkElement {

  lazy val leadingByte: Byte = if (isEndpoint) {
    1.toByte
  } else {
    0.toByte
  }

  override def bytes: ByteVector = {
    ByteVector(leadingByte) ++
      BigSizeUInt(outcome).bytes ++
      UInt64(value.toLong).bytes ++
      UInt16(extraPrecision).bytes
  }

  def toOutcomePayoutPoint: OutcomePayoutPoint = {
    OutcomePayoutPoint(outcome, value)
  }
}

object OldTLVPoint extends Factory[OldTLVPoint] {

  override def fromBytes(bytes: ByteVector): OldTLVPoint = {
    val isEndpoint = bytes.head match {
      case 0 => false
      case 1 => true
      case b: Byte =>
        throw new IllegalArgumentException(
          s"Did not recognize leading byte: $b")
    }

    val outcome = BigSizeUInt(bytes.tail)
    val value = UInt64(bytes.drop(1 + outcome.byteSize).take(8))
    val extraPrecision = UInt16(bytes.drop(9 + outcome.byteSize).take(2)).toInt

    OldTLVPoint(outcome = outcome.toLong,
                value = Satoshis(value.toLong),
                extraPrecision = extraPrecision,
                isEndpoint = isEndpoint)
  }
}

sealed trait PayoutCurvePieceTLV extends DLCSubType

object PayoutCurvePieceTLV extends Factory[PayoutCurvePieceTLV] {

  val allFactories: Vector[Factory[PayoutCurvePieceTLV]] =
    Vector(PolynomialPayoutCurvePieceTLV, HyperbolaPayoutCurvePieceTLV)

  private val knownSubTypes: Vector[Byte] = {
    Vector(PolynomialPayoutCurvePieceTLV.subType,
           HyperbolaPayoutCurvePieceTLV.subType)
  }

  private def fromOldTLVValue(bytes: ByteVector): PayoutCurvePieceTLV = {
    PolynomialPayoutCurvePieceTLV
      .fromBytesT(bytes)
      .getOrElse(HyperbolaPayoutCurvePieceTLV.fromBytes(bytes))
  }

  private val subTypeMap: Map[Byte, Factory[PayoutCurvePieceTLV]] = {
    knownSubTypes.zip(allFactories).toMap
  }

  override def fromBytes(bytes: ByteVector): PayoutCurvePieceTLV = {
    val t = Try {
      SubTypeUtil.fromBytes(bytes, subTypeMap)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }
}

case class PolynomialPayoutCurvePieceTLV(
    midpoints: Vector[TLVPoint],
    serializationVersion: DLCSerializationVersion)
    extends PayoutCurvePieceTLV {
  override val subType: Byte = PolynomialPayoutCurvePieceTLV.subType

  override val bytes: ByteVector = {
    ByteVector.fromByte(subType) ++
      TLVUtil.bigSizePrefixedList(midpoints)
  }

  override val byteSize: Long = {
    serializationVersion match {
      case DLCSerializationVersion.Gamma => super.byteSize
      case DLCSerializationVersion.Alpha | DLCSerializationVersion.Beta =>
        val value = TLVUtil.u16PrefixedList(midpoints)
        val len = BigSizeUInt.calcFor(value)
        PolynomialPayoutCurvePieceTLV.oldTpe.byteSize + len.byteSize + value.size
    }
  }
}

object PolynomialPayoutCurvePieceTLV
    extends Factory[PolynomialPayoutCurvePieceTLV] {
  val oldTpe: BigSizeUInt = BigSizeUInt(42792)

  final val subType: Byte = 0.toByte

  def fromOldTLVValue(bytes: ByteVector): PolynomialPayoutCurvePieceTLV = {
    val TLV.DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)
    require(
      tpe == oldTpe,
      s"Incorrect tpe for PolynomialPayoutCurvePieceTLV, got=$tpe, expected=$oldTpe")

    val iter = ValueIterator(value)

    val points = iter.takeU16PrefixedList(() => iter.take(TLVPoint))
    PolynomialPayoutCurvePieceTLV(points, DLCSerializationVersion.Beta)

  }

  override def fromBytes(bytes: ByteVector): PolynomialPayoutCurvePieceTLV = {
    val t = Try {
      require(
        bytes.head == subType,
        s"Required subtype is $subType for ContractDescriptorV0TLV, got=${bytes.head}")
      val iter = ValueIterator(bytes.drop(1))

      val points = iter.takeBigSizePrefixedList(() => iter.take(TLVPoint))

      PolynomialPayoutCurvePieceTLV(points, DLCSerializationVersion.Gamma)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }

  //override val typeName: String = "PolynomialPayoutCurvePieceTLV"
}

case class Signed16PTLVNumber(
    sign: Boolean,
    withoutPrecision: BigInt,
    extraPrecision: Int)
    extends NetworkElement {

  lazy val toBigDecimal: BigDecimal = {
    val absVal =
      BigDecimal(withoutPrecision) + (BigDecimal(extraPrecision) / (1 << 16))

    if (sign) absVal else -absVal
  }

  lazy val signByte: Byte = if (sign) {
    1.toByte
  } else {
    0.toByte
  }

  override def bytes: ByteVector = {
    ByteVector(signByte) ++
      UInt64(withoutPrecision).bytes ++
      UInt16(extraPrecision).bytes
  }

  override def toString: String = {
    s"Signed16PTLVNumber(sign=$sign,withoutPrecision=$withoutPrecision,extraPrecision=$extraPrecision,toBigDecimal=$toBigDecimal)"
  }
}

object Signed16PTLVNumber extends Factory[Signed16PTLVNumber] {

  override def fromBytes(bytes: ByteVector): Signed16PTLVNumber = {
    val iter = ValueIterator(bytes)
    val sign = iter.takeBoolean()

    val withoutPrecision = iter.takeU64()
    val extraPrecision = iter.takeU16()

    Signed16PTLVNumber(sign, withoutPrecision.toBigInt, extraPrecision.toInt)
  }

  def fromBigDecimal(number: BigDecimal): Signed16PTLVNumber = {
    val sign = number >= 0
    val withoutPrecision =
      number.abs.setScale(0, RoundingMode.FLOOR).toLongExact
    val extraPrecisionBD = (number.abs - withoutPrecision) * (1 << 16)
    val extraPrecision =
      extraPrecisionBD.setScale(0, RoundingMode.FLOOR).toIntExact

    Signed16PTLVNumber(sign, withoutPrecision, extraPrecision)
  }
}

case class HyperbolaPayoutCurvePieceTLV(
    usePositivePiece: Boolean,
    translateOutcome: Signed16PTLVNumber,
    translatePayout: Signed16PTLVNumber,
    a: Signed16PTLVNumber,
    b: Signed16PTLVNumber,
    c: Signed16PTLVNumber,
    d: Signed16PTLVNumber,
    serializationVersion: DLCSerializationVersion)
    extends PayoutCurvePieceTLV {

  override val subType: Byte = HyperbolaPayoutCurvePieceTLV.subType

  override val bytes: ByteVector = {
    ByteVector.fromByte(subType) ++
      TLVUtil.boolBytes(usePositivePiece) ++
      translateOutcome.bytes ++
      translatePayout.bytes ++
      a.bytes ++
      b.bytes ++
      c.bytes ++
      d.bytes
  }

  override val byteSize: Long = {
    serializationVersion match {
      case DLCSerializationVersion.Gamma => super.byteSize
      case DLCSerializationVersion.Alpha | DLCSerializationVersion.Beta =>
        val value = bytes.drop(1) //same value just need to drop subtype byte
        val len = BigSizeUInt.calcFor(value)
        HyperbolaPayoutCurvePieceTLV.oldTpe.byteSize + len.byteSize + value.size
    }
  }
}

object HyperbolaPayoutCurvePieceTLV
    extends Factory[HyperbolaPayoutCurvePieceTLV] {
  val oldTpe: BigSizeUInt = BigSizeUInt(42794)

  final val subType: Byte = 1.toByte

  def fromOldTLVValue(bytes: ByteVector): HyperbolaPayoutCurvePieceTLV = {

    val TLV.DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)

    require(
      tpe == oldTpe,
      s"Incorrect tpe for PolynomialPayoutCurvePieceTLV, got=$tpe, expected=$oldTpe")

    val iter = ValueIterator(value)

    val usePositivePiece = iter.takeBoolean()
    val translateOutcome = iter.take(Signed16PTLVNumber)
    val translatePayout = iter.take(Signed16PTLVNumber)
    val a = iter.take(Signed16PTLVNumber)
    val b = iter.take(Signed16PTLVNumber)
    val c = iter.take(Signed16PTLVNumber)
    val d = iter.take(Signed16PTLVNumber)

    HyperbolaPayoutCurvePieceTLV(usePositivePiece,
                                 translateOutcome,
                                 translatePayout,
                                 a,
                                 b,
                                 c,
                                 d,
                                 DLCSerializationVersion.Beta)
  }

  override def fromBytes(bytes: ByteVector): HyperbolaPayoutCurvePieceTLV = {
    val t = Try {
      require(
        bytes.head == subType,
        s"Required subtype is $subType for HyperbolaPayoutCurvePieceTLV, got=${bytes.head}")
      val iter = ValueIterator(bytes.drop(1))

      val usePositivePiece = iter.takeBoolean()

      val translateOutcome = iter.take(Signed16PTLVNumber)

      val translatePayout = iter.take(Signed16PTLVNumber)

      val a = iter.take(Signed16PTLVNumber)
      val b = iter.take(Signed16PTLVNumber)
      val c = iter.take(Signed16PTLVNumber)
      val d = iter.take(Signed16PTLVNumber)
      HyperbolaPayoutCurvePieceTLV(usePositivePiece = usePositivePiece,
                                   translateOutcome = translateOutcome,
                                   translatePayout = translatePayout,
                                   a = a,
                                   b = b,
                                   c = c,
                                   d = d,
                                   DLCSerializationVersion.Gamma)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }
}

case class OldPayoutFunctionV0TLV(points: Vector[OldTLVPoint])
    extends DLCSetupPieceTLV {
  override val tpe: BigSizeUInt = PayoutFunctionV0TLV.oldTpe

  override val value: ByteVector = u16PrefixedList(points)
}

/** @see https://github.com/discreetlogcontracts/dlcspecs/blob/8ee4bbe816c9881c832b1ce320b9f14c72e3506f/NumericOutcome.md#curve-serialization */
case class PayoutFunctionV0TLV(
    endpoints: Vector[TLVPoint],
    pieces: Vector[PayoutCurvePieceTLV],
    serializationVersion: DLCSerializationVersion)
    extends DLCPlainType {
  require(
    endpoints.length == pieces.length + 1,
    s"Number of endpoints (${endpoints.length}) does not match number of pieces (${pieces.length}).")

  override val bytes: ByteVector = {
    TLVUtil.bigSizePrefixedList[(TLVPoint, PayoutCurvePieceTLV)](
      endpoints.init.zip(pieces),
      { case (leftEndpoint: TLVPoint, piece: PayoutCurvePieceTLV) =>
        leftEndpoint.bytes ++ piece.bytes
      }) ++ endpoints.last.bytes
  }

  def piecewisePolynomialEndpoints: Vector[PiecewisePolynomialEndpoint] = {
    endpoints.map(e => PiecewisePolynomialEndpoint(e.outcome, e.value))
  }

  override val byteSize: Long = {
    serializationVersion match {
      case DLCSerializationVersion.Alpha =>
        val old = OldPayoutFunctionV0TLV(endpoints.map(p =>
          OldTLVPoint(p.outcome, p.value, p.extraPrecision, true)))
        old.byteSize
      case DLCSerializationVersion.Beta =>
        val valueByteSize = {
          val endPointsAndPieces = endpoints.zip(pieces)
          val prefix = UInt16(endpoints.size + 1)
          val byteSize: Long =
            endPointsAndPieces.map {
              case (leftEndpoint: TLVPoint, piece: PayoutCurvePieceTLV) =>
                leftEndpoint.byteSize + piece.byteSize
            }.sum + endpoints.last.byteSize

          prefix.byteSize + byteSize
        }
        PayoutFunctionV0TLV.oldTpe.byteSize + BigSizeUInt(
          valueByteSize).byteSize + valueByteSize
      case DLCSerializationVersion.Gamma =>
        super.byteSize
    }
  }
}

object PayoutFunctionV0TLV extends Factory[PayoutFunctionV0TLV] {
  val oldTpe: BigSizeUInt = BigSizeUInt(42790)

  private def fromTLVValue(bytes: ByteVector): PayoutFunctionV0TLV = {
    val t = Try {
      val TLV.DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)
      require(tpe == oldTpe,
              s"Incorrect tpe for PayoutFunctionV0TLV, got=$tpe, expected=$tpe")

      val iter = ValueIterator(value)

      val endpointsAndPieces = iter.takeU16PrefixedList { () =>
        val leftEndpoint = iter.take(TLVPoint)
        val piece = iter.take(PayoutCurvePieceTLV)
        (leftEndpoint, piece)
      }
      val rightEndpoint = iter.take(TLVPoint)
      val endpoints = endpointsAndPieces.map(_._1).:+(rightEndpoint)
      val pieces = endpointsAndPieces.map(_._2)
      PayoutFunctionV0TLV(endpoints,
                          pieces,
                          serializationVersion = DLCSerializationVersion.Beta)
    }

    t.getOrElse(fromAlphaTLVValue(bytes))
  }

  override def fromBytes(bytes: ByteVector): PayoutFunctionV0TLV = {
    val iter = ValueIterator(bytes)
    val t = Try {
      val endpointsAndPieces = iter.takeBigSizePrefixedList { () =>
        val leftEndpoint = iter.take(TLVPoint)
        val piece = iter.take(PayoutCurvePieceTLV)
        (leftEndpoint, piece)
      }
      val rightEndpoint = iter.take(TLVPoint)
      val endpoints = endpointsAndPieces.map(_._1).:+(rightEndpoint)
      val pieces = endpointsAndPieces.map(_._2)

      val p = PayoutFunctionV0TLV(endpoints,
                                  pieces,
                                  serializationVersion =
                                    DLCSerializationVersion.Gamma)
      p
    }
    t.getOrElse(fromTLVValue(bytes))
  }

  private def fromAlphaTLVValue(bytes: ByteVector): PayoutFunctionV0TLV = {
    val TLV.DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)
    require(tpe == oldTpe,
            s"Incorrect tpe for PayoutFunctionV0TLV, got=$tpe, expected=$tpe")
    val iter = ValueIterator(value)
    val points = iter.takeU16PrefixedList(() => iter.take(OldTLVPoint))
    DLCPayoutCurve.fromPointsPre144(points).toSubType
  }
}

case class ContractDescriptorV1TLV(
    numDigits: Int,
    payoutFunction: PayoutFunctionV0TLV,
    roundingIntervals: RoundingIntervalsV0TLV,
    serializationVersion: DLCSerializationVersion)
    extends ContractDescriptorTLV {

  val numDigitsU16: UInt16 = UInt16(numDigits)
  override val subType: Byte = ContractDescriptorV1TLV.subType

  override val bytes: ByteVector = {
    ByteVector.fromByte(subType) ++
      numDigitsU16.bytes ++
      payoutFunction.bytes ++
      roundingIntervals.bytes
  }

  override val byteSize: Long = {
    payoutFunction.serializationVersion match {
      case DLCSerializationVersion.Gamma =>
        super.byteSize
      case DLCSerializationVersion.Beta =>
        //use to be TLV, so need this for backwards compatability

        val payloadSize =
          numDigitsU16.byteSize + payoutFunction.byteSize + roundingIntervals.byteSize
        val total =
          ContractDescriptorV1TLV.oldTpe.byteSize + BigSizeUInt(
            payloadSize).byteSize + payloadSize
        total
      case DLCSerializationVersion.Alpha =>
        //use to be TLV, so need this for backwards compatability

        val payloadSize =
          numDigitsU16.byteSize + payoutFunction.byteSize + roundingIntervals.byteSize
        val total =
          ContractDescriptorV1TLV.oldTpe.byteSize + BigSizeUInt(
            payloadSize).byteSize + payloadSize
        total
    }
  }
}

object ContractDescriptorV1TLV extends Factory[ContractDescriptorV1TLV] {
  final val subType: Byte = 1.toByte

  val oldTpe: BigSizeUInt = BigSizeUInt(42784)

  private def fromOldTLVValue(bytes: ByteVector): ContractDescriptorV1TLV = {
    val DecodeTLVResult(tpe, len, value) = TLV.decodeTLV(bytes)
    require(
      tpe == oldTpe,
      s"Invalid type $tpe (${TLV.getTypeName(tpe)}) len=$len when expecting ${this.oldTpe}")
    val iter = ValueIterator(value)

    val numDigits = iter.takeU16()

    val payoutFunction = iter.take(PayoutFunctionV0TLV)

    val roundingIntervals = iter.take(RoundingIntervalsV0TLV)

    ContractDescriptorV1TLV(numDigits.toInt,
                            payoutFunction,
                            roundingIntervals,
                            DLCSerializationVersion.Beta)
  }

  override def fromBytes(bytes: ByteVector): ContractDescriptorV1TLV = {
    val t = Try {
      require(
        bytes.head == subType,
        s"Required subtype is $subType for ContractDescriptorV1TLV, got=${bytes.head}")

      val iter = ValueIterator(bytes.drop(1))
      val numDigits = iter.takeU16()

      val payoutFunction = iter.take(PayoutFunctionV0TLV)
      val roundingIntervals = iter.take(RoundingIntervalsV0TLV)

      ContractDescriptorV1TLV(numDigits.toInt,
                              payoutFunction,
                              roundingIntervals,
                              DLCSerializationVersion.Gamma)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }

}

sealed trait OracleInfoTLV extends DLCSubType {
  def announcements: Vector[OracleAnnouncementTLV]
}

object OracleInfoTLV extends Factory[OracleInfoTLV] {

  val allFactories: Vector[Factory[OracleInfoTLV]] =
    Vector(OracleInfoV0TLV, OracleInfoV1TLV)

  private val knownSubTypes: Vector[Byte] = {
    Vector(OracleInfoV0TLV.subType, OracleInfoV1TLV.subType)
  }

  private val subTypeMap: Map[Byte, Factory[OracleInfoTLV]] = {
    knownSubTypes.zip(allFactories).toMap
  }

  private def fromOldTLVValue(bytes: ByteVector): OracleInfoTLV = {
    OracleInfoV0TLV
      .fromBytesT(bytes)
      .getOrElse(OracleInfoV1TLV.fromBytes(bytes))
  }

  override def fromBytes(bytes: ByteVector): OracleInfoTLV = {
    val t = Try {
      SubTypeUtil.fromBytes(bytes, subTypeMap)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }
}

case class OracleInfoV0TLV(
    announcement: OracleAnnouncementTLV,
    serializationVersion: DLCSerializationVersion)
    extends OracleInfoTLV {

  override val announcements: Vector[OracleAnnouncementTLV] = {
    Vector(announcement)
  }

  override val subType: Byte = OracleInfoV0TLV.subType

  override val bytes: ByteVector = {
    ByteVector.fromByte(subType) ++ announcement.bytes
  }

  override val byteSize: Long = serializationVersion match {
    case DLCSerializationVersion.Gamma => super.byteSize
    case DLCSerializationVersion.Alpha | DLCSerializationVersion.Beta =>
      OracleInfoV0TLV.oldTpe.byteSize + BigSizeUInt
        .calcFor(announcement.bytes)
        .byteSize + announcement.byteSize
  }
}

object OracleInfoV0TLV extends Factory[OracleInfoV0TLV] {

  final val subType: Byte = 0.toByte
  val oldTpe: BigSizeUInt = BigSizeUInt(42770)

  private def fromOldTLVValue(bytes: ByteVector): OracleInfoV0TLV = {
    val DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)
    require(
      tpe == oldTpe,
      s"Tpe was not correct for OracleInfoV0TLV, got=$tpe, expected=$oldTpe")
    val iter = ValueIterator(value)
    val announcements = iter.take(OracleAnnouncementTLV)
    val oi = OracleInfoV0TLV(announcements, DLCSerializationVersion.Beta)
    oi
  }

  override def fromBytes(bytes: ByteVector): OracleInfoV0TLV = {
    val t = Try {
      require(
        bytes.head == subType,
        s"Required subtype is $subType for OracleInfoV0TLV, got=${bytes.head}")
      val iter = ValueIterator(bytes.drop(1))

      val announcement = iter.take(OracleAnnouncementTLV)

      OracleInfoV0TLV(announcement, DLCSerializationVersion.Gamma)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }
}

sealed trait MultiOracleInfoTLV extends OracleInfoTLV {
  def threshold: Int
  def oracles: OrderedAnnouncements

  override val announcements: Vector[OracleAnnouncementTLV] = {
    oracles.toVector
  }
}

case class OracleInfoV1TLV(
    threshold: Int,
    oracles: OrderedAnnouncements,
    oracleParamsOpt: OptionTLV[OracleParamsV0TLV],
    serializationVersion: DLCSerializationVersion)
    extends MultiOracleInfoTLV {
  serializationVersion match {
    case DLCSerializationVersion.Alpha | DLCSerializationVersion.Beta =>
      require(
        oracleParamsOpt.isEmpty,
        s"Cannot have an old serialization and have oracleParams defined for OracleInfoV0TLV")
    case DLCSerializationVersion.Gamma =>
    //no invariant if we are using the gamma serialization
  }
  override val subType: Byte = OracleInfoV1TLV.subType

  override val bytes: ByteVector = {
    ByteVector.fromByte(subType) ++
      UInt16(threshold).bytes ++
      TLVUtil.bigSizePrefixedList(oracles.toVector) ++
      oracleParamsOpt.bytes
  }

  override val byteSize: Long = {
    serializationVersion match {
      case DLCSerializationVersion.Alpha | DLCSerializationVersion.Beta =>
        val value =
          UInt16(threshold).bytes ++ TLVUtil.u16PrefixedList(oracles.toVector)
        oldTpe.byteSize + BigSizeUInt.calcFor(value).byteSize + value.size
      case DLCSerializationVersion.Gamma =>
        super.byteSize
    }
  }
}

object OracleInfoV1TLV extends Factory[OracleInfoV1TLV] {
  final val subType: Byte = 1.toByte
  val oldTpe: BigSizeUInt = BigSizeUInt(42786)

  private def fromOldTLVValue(bytes: ByteVector): OracleInfoV1TLV = {
    val DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)
    require(tpe == oldTpe,
            s"Incorrect tpe for OracleInfoV1TLV, got=$tpe expected=$oldTpe")
    val iter = ValueIterator(value)
    val threshold = iter.takeU16().toInt
    val oracles =
      iter.takeU16PrefixedList(() => iter.take(OracleAnnouncementTLV))
    OracleInfoV1TLV(threshold,
                    OrderedAnnouncements(oracles),
                    NoneTLV,
                    DLCSerializationVersion.Beta)
  }

  override def fromBytes(bytes: ByteVector): OracleInfoV1TLV = {
    val t = Try {
      require(
        bytes.head == subType,
        s"Required subtype is $subType for OracleInfoV1TLV, got=${bytes.head}")

      val iter = ValueIterator(bytes.drop(1))

      val threshold = iter.takeU16().toInt
      val oracles =
        iter.takeBigSizePrefixedList(() => iter.take(OracleAnnouncementTLV))
      val paramsOpt = iter.takeOpt(OracleParamsV0TLV)
      OracleInfoV1TLV(threshold,
                      OrderedAnnouncements(oracles),
                      paramsOpt,
                      DLCSerializationVersion.Gamma)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }
}

sealed trait OracleParamsTLV extends NetworkElement

case class OracleParamsV0TLV(
    maxErrorExp: Int,
    minFailExp: Int,
    maximizeCoverage: Boolean)
    extends OracleParamsTLV {

  override val bytes: ByteVector = {
    UInt16(maxErrorExp).bytes ++
      UInt16(minFailExp).bytes ++
      TLVUtil.boolBytes(maximizeCoverage)
  }
}

trait FactoryOptionTLV[T <: NetworkElement] extends Factory[OptionTLV[T]] {

  override def fromBytes(bytes: ByteVector): OptionTLV[T]
}

object OracleParamsV0TLV extends FactoryOptionTLV[OracleParamsV0TLV] {

  override def fromBytes(bytes: ByteVector): OptionTLV[OracleParamsV0TLV] = {
    if (bytes.head == 0) {
      NoneTLV
    } else {
      require(
        bytes.head == 1,
        s"OracleParamsV0TLV has wrong optional subtype, got=${bytes.head}")
      val iter = ValueIterator(bytes.drop(1))

      val maxErrorExp = iter.takeU16().toInt
      val minFailExp = iter.takeU16().toInt
      val maximizeCoverage = iter.takeBoolean()

      val p = OracleParamsV0TLV(maxErrorExp, minFailExp, maximizeCoverage)
      SomeTLV(p)
    }

  }
}

sealed trait ContractInfoTLV extends DLCSubType {
  def totalCollateral: Satoshis
}

object ContractInfoTLV extends Factory[ContractInfoTLV] {

  private val allFactories: Vector[DLCSubTypeFactory[ContractInfoTLV]] = {
    Vector(ContractInfoV0TLV, ContractInfoV1TLV)
  }

  private val knownSubTypes: Vector[Byte] = {
    Vector(NoNegotiationFieldsTLV.subType, NegotiationFieldsV1TLV.subType)
  }

  private val subTypeMap: Map[Byte, DLCSubTypeFactory[ContractInfoTLV]] = {
    knownSubTypes.zip(allFactories).toMap
  }

  private def fromOldTLVValue(bytes: ByteVector): ContractInfoTLV = {
    Try(ContractInfoV0TLV.fromBytes(bytes))
      .getOrElse(ContractInfoV1TLV.fromBytes(bytes))
  }

  override def fromBytes(bytes: ByteVector): ContractInfoTLV = {
    val t = Try {
      SubTypeUtil.fromBytes(bytes, subTypeMap)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }
}

case class ContractInfoV0TLV(
    totalCollateral: Satoshis,
    contractDescriptor: ContractDescriptorTLV,
    oracleInfo: OracleInfoTLV,
    serializationVersion: DLCSerializationVersion)
    extends ContractInfoTLV {

  override val subType: Byte = ContractInfoV0TLV.subType

  override val bytes: ByteVector = {
    ByteVector.fromByte(subType) ++
      TLVUtil.satBytes(totalCollateral) ++
      contractDescriptor.bytes ++
      oracleInfo.bytes
  }

  override val byteSize: Long = {
    serializationVersion match {
      case DLCSerializationVersion.Gamma => super.byteSize
      case DLCSerializationVersion.Alpha | DLCSerializationVersion.Beta =>
        val valueByteSize = TLVUtil.satBytes(totalCollateral).size +
          contractDescriptor.byteSize +
          oracleInfo.byteSize
        oldTpe.byteSize + BigSizeUInt(valueByteSize).byteSize + valueByteSize
    }
  }
}

object ContractInfoV0TLV extends DLCSubTypeFactory[ContractInfoV0TLV] {

  val oldTpe: BigSizeUInt = BigSizeUInt(55342)
  final val subType: Byte = 0.toByte

  lazy val dummy: ContractInfoV0TLV = {
    ContractInfoV0TLV(
      Satoshis.zero,
      ContractDescriptorV0TLV(Vector("dummy" -> Satoshis(10000)),
                              DLCSerializationVersion.current),
      OracleInfoV0TLV(OracleAnnouncementV0TLV.dummy,
                      DLCSerializationVersion.current),
      DLCSerializationVersion.current
    )
  }

  private def fromOldTLVValue(bytes: ByteVector): ContractInfoV0TLV = {
    val DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)
    require(tpe == oldTpe,
            s"Incorrect tpe for ContractInfoV0TLV, got=$tpe, expected=$oldTpe")

    val iter = ValueIterator(value)
    val totalCollateral = iter.takeSats()
    val contractDescriptor = iter.take(ContractDescriptorTLV)
    val oracleInfo = iter.take(OracleInfoTLV)

    ContractInfoV0TLV(totalCollateral,
                      contractDescriptor,
                      oracleInfo,
                      DLCSerializationVersion.Beta)
  }

  override def fromBytes(bytes: ByteVector): ContractInfoV0TLV = {
    val t = Try {
      require(bytes.head == subType,
              s"ContractInfoV0TLV expected subType=$subType got=${bytes.head}")

      val iter = ValueIterator(bytes.drop(1))

      val totalCollateral = iter.takeSats()
      val contractDescriptor = iter.take(ContractDescriptorTLV)
      val oracleInfo = iter.take(OracleInfoTLV)

      ContractInfoV0TLV(totalCollateral,
                        contractDescriptor,
                        oracleInfo,
                        DLCSerializationVersion.Gamma)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }
}

case class ContractInfoV1TLV(
    totalCollateral: Satoshis,
    contractOraclePairs: Vector[(ContractDescriptorTLV, OracleInfoTLV)])
    extends ContractInfoTLV {
  override val subType: Byte = ContractInfoV1TLV.subType

  override val bytes: ByteVector = {
    ByteVector.fromByte(subType) ++ TLVUtil.satBytes(totalCollateral) ++
      TLVUtil.bigSizePrefixedList[(ContractDescriptorTLV, OracleInfoTLV)](
        contractOraclePairs,
        { case (descriptor, oracleInfo) =>
          descriptor.bytes ++ oracleInfo.bytes
        }
      )
  }
}

object ContractInfoV1TLV extends DLCSubTypeFactory[ContractInfoV1TLV] {

  final val subType: Byte = 1.toByte

  override def fromBytes(bytes: ByteVector): ContractInfoV1TLV = {
    require(bytes.head == subType,
            s"ContractInfoV1TLV expected subType=$subType got=${bytes.head}")

    val iter = ValueIterator(bytes.drop(1))

    val totalCollateral = iter.takeSats()

    val contracts = iter.takeBigSizePrefixedList { () =>
      val descriptor = iter.take(ContractDescriptorTLV)

      val oracleInfo = iter.take(OracleInfoTLV)

      (descriptor, oracleInfo)
    }

    ContractInfoV1TLV(totalCollateral, contracts)
  }
}

sealed trait FundingInputTLV extends DLCPlainType {
  def inputSerialId: UInt64
}

case class FundingInputV0TLV(
    inputSerialId: UInt64,
    prevTx: Transaction,
    prevTxVout: UInt32,
    sequence: UInt32,
    maxWitnessLen: UInt16,
    redeemScriptOpt: Option[WitnessScriptPubKey],
    serializationVersion: DLCSerializationVersion)
    extends FundingInputTLV {

  lazy val output: TransactionOutput = prevTx.outputs(prevTxVout.toInt)

  lazy val outPoint: TransactionOutPoint =
    TransactionOutPoint(prevTx.txId, prevTxVout)

  lazy val input: TransactionInput = {
    val scriptSig = redeemScriptOpt match {
      case Some(redeemScript) => P2SHScriptSignature(redeemScript)
      case None               => EmptyScriptSignature
    }

    TransactionInput(outPoint, scriptSig, sequence)
  }

  lazy val outputReference: OutputReference = OutputReference(outPoint, output)

  override val bytes: ByteVector = {
    val redeemScript =
      redeemScriptOpt.getOrElse(EmptyScriptPubKey)

    inputSerialId.bytes ++
      TLVUtil.bigSizePrefix(prevTx.bytes) ++
      prevTxVout.bytes ++
      sequence.bytes ++
      maxWitnessLen.bytes ++
      TLV.encodeScript(redeemScript)
  }

  override val byteSize: Long = {
    serializationVersion match {
      case DLCSerializationVersion.Alpha | DLCSerializationVersion.Beta =>
        //have to use the old serialization format to correctly
        //compute byteSize so we can properly deserialize the data structure
        val redeemScript =
          redeemScriptOpt.getOrElse(EmptyScriptPubKey)
        val value = inputSerialId.bytes ++
          TLVUtil.u16Prefix(prevTx.bytes) ++
          prevTxVout.bytes ++
          sequence.bytes ++
          maxWitnessLen.bytes ++
          TLV.encodeScript(redeemScript)
        val len = BigSizeUInt.calcFor(value)
        FundingInputV0TLV.oldTpe.byteSize + len.byteSize + value.size
      case DLCSerializationVersion.Gamma =>
        super.byteSize
    }
  }
}

object FundingInputV0TLV extends Factory[FundingInputV0TLV] {
  val oldTpe: BigSizeUInt = BigSizeUInt(42772)

  private def fromTLVValue(bytes: ByteVector): FundingInputV0TLV = {
    val DecodeTLVResult(parsedTpe, _, value) = TLV.decodeTLV(bytes)

    require(
      parsedTpe == oldTpe,
      s"Got parsedTpe=$parsedTpe oldTpe=$oldTpe type when expecting FundingInputV0TLV")

    val iter = ValueIterator(value)

    val serialId = iter.takeU64()
    val prevTx = iter.takeU16Prefixed(iter.take(Transaction, _))
    val prevTxVout = iter.takeU32()
    val sequence = iter.takeU32()
    val maxWitnessLen = iter.takeU16()
    val redeemScript = iter.takeSPK()
    val redeemScriptOpt = redeemScript match {
      case EmptyScriptPubKey         => None
      case wspk: WitnessScriptPubKey => Some(wspk)
      case _: NonWitnessScriptPubKey =>
        throw new IllegalArgumentException(
          s"Redeem Script must be Segwit SPK: $redeemScript")
    }

    val f = FundingInputV0TLV(
      inputSerialId = serialId,
      prevTx = prevTx,
      prevTxVout = prevTxVout,
      sequence = sequence,
      maxWitnessLen = maxWitnessLen,
      redeemScriptOpt = redeemScriptOpt,
      serializationVersion = DLCSerializationVersion.Beta
    )

    f
  }

  override def fromBytes(bytes: ByteVector): FundingInputV0TLV = {
    val t = Try {
      val iter = ValueIterator(bytes)

      val serialId = iter.takeU64()
      val prevTx = iter.takeBigSizePrefixed(iter.take(Transaction, _))
      val prevTxVout = iter.takeU32()
      val sequence = iter.takeU32()
      val maxWitnessLen = iter.takeU16()
      val redeemScript = iter.takeSPK()
      val redeemScriptOpt = redeemScript match {
        case EmptyScriptPubKey         => None
        case wspk: WitnessScriptPubKey => Some(wspk)
        case _: NonWitnessScriptPubKey =>
          throw new IllegalArgumentException(
            s"Redeem Script must be Segwit SPK: $redeemScript")
      }

      FundingInputV0TLV(
        inputSerialId = serialId,
        prevTx = prevTx,
        prevTxVout = prevTxVout,
        sequence = sequence,
        maxWitnessLen = maxWitnessLen,
        redeemScriptOpt = redeemScriptOpt,
        serializationVersion = DLCSerializationVersion.Gamma
      )
    }

    t.getOrElse(fromTLVValue(bytes))
  }

  val dummy: FundingInputV0TLV = FundingInputV0TLV(
    UInt64.zero,
    EmptyTransaction,
    prevTxVout = UInt32.zero,
    UInt32.zero,
    UInt16.zero,
    None,
    DLCSerializationVersion.current)
}

sealed trait CETSignaturesTLV extends DLCPlainType

case class CETSignaturesV0TLV(
    sigs: Vector[ECAdaptorSignature],
    serializationVersion: DLCSerializationVersion)
    extends CETSignaturesTLV {

  override val bytes: ByteVector = {
    TLVUtil.bigSizePrefixedList(sigs)
  }

  override val byteSize: Long = {
    serializationVersion match {
      case DLCSerializationVersion.Alpha | DLCSerializationVersion.Beta =>
        val value = bytes
        val len = BigSizeUInt.calcFor(value)
        FundingInputV0TLV.oldTpe.byteSize + len.byteSize + value.size
      case DLCSerializationVersion.Gamma =>
        super.byteSize
    }
  }

  override def toString: String = {
    if (sigs.length <= 5) {
      super.toString
    } else {
      s"CETSignaturesV0TLV(sigs=${sigs.take(
        2)}..., omitting remainingSigs of length=${sigs.length - 2})"
    }
  }
}

object CETSignaturesV0TLV extends Factory[CETSignaturesV0TLV] {
  val oldTpe: BigSizeUInt = BigSizeUInt(42774)

  private def fromOldTLVValue(bytes: ByteVector): CETSignaturesV0TLV = {
    val DecodeTLVResult(parsedTpe, _, value) = TLV.decodeTLV(bytes)

    require(
      parsedTpe == oldTpe,
      s"Got ${TLV.getTypeName(oldTpe)} type when expecting FundingInputV0TLV")
    val iter = ValueIterator(value)

    val sigs =
      iter.takeBigSizePrefixedList(() => iter.take(ECAdaptorSignature, 162))

    CETSignaturesV0TLV(sigs, DLCSerializationVersion.Beta)
  }

  override def fromBytes(bytes: ByteVector): CETSignaturesV0TLV = {
    val t = Try {
      val iter = ValueIterator(bytes)

      val sigs =
        iter.takeBigSizePrefixedList(() => iter.take(ECAdaptorSignature, 162))

      CETSignaturesV0TLV(sigs, DLCSerializationVersion.Gamma)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }
}

sealed trait FundingSignaturesTLV extends DLCPlainType

case class FundingSignaturesV0TLV(
    witnesses: Vector[ScriptWitnessV0],
    serializationVersion: DLCSerializationVersion)
    extends FundingSignaturesTLV {

  override val bytes: ByteVector = {
    TLVUtil.bigSizePrefixedList(
      witnesses,
      { witness: ScriptWitnessV0 =>
        val witBytes =
          TLVUtil.bigSizePrefixedList[ByteVector](
            witness.stack.toVector.reverse,
            TLVUtil.bigSizePrefix)
        witBytes
      }
    )
  }

  override val byteSize: Long = {
    serializationVersion match {
      case DLCSerializationVersion.Alpha | DLCSerializationVersion.Beta =>
        val value = TLVUtil.u16PrefixedList(
          witnesses,
          { witness: ScriptWitnessV0 =>
            val witBytes =
              TLVUtil.u16PrefixedList[ByteVector](
                witness.stack.toVector.reverse,
                TLVUtil.bigSizePrefix)
            witBytes
          })
        val len = BigSizeUInt.calcFor(value)
        FundingSignaturesV0TLV.oldTpe.byteSize + len.toInt + value.size
      case DLCSerializationVersion.Gamma =>
        super.byteSize
    }
  }
}

object FundingSignaturesV0TLV extends Factory[FundingSignaturesV0TLV] {
  val oldTpe: BigSizeUInt = BigSizeUInt(42776)

  private def fromOldTLVValue(bytes: ByteVector): FundingSignaturesV0TLV = {

    val DecodeTLVResult(tpe, _, tlv) = TLV.decodeTLV(bytes)

    require(tpe == oldTpe,
            s"Bad tpe for FundingSignaturesV0TLV, got=$tpe, expected=$oldTpe")
    val iter = ValueIterator(tlv)
    val witnesses = iter.takeU16PrefixedList { () =>
      val stack =
        iter.takeU16PrefixedList(() => iter.takeU16Prefixed(iter.take))

      ScriptWitness(stack.reverse) match {
        case EmptyScriptWitness =>
          throw new IllegalArgumentException(s"Invalid witness: $stack")
        case witness: ScriptWitnessV0 => witness
        case taprootWitness: TaprootWitness =>
          throw new IllegalArgumentException(
            s"Invalid witness, taproot not supported in DLC spec, got=$taprootWitness")
      }
    }

    FundingSignaturesV0TLV(witnesses, DLCSerializationVersion.Beta)
  }

  override def fromBytes(bytes: ByteVector): FundingSignaturesV0TLV = {
    val t = Try {
      val iter = ValueIterator(bytes)

      val witnesses = iter.takeBigSizePrefixedList { () =>
        val stack =
          iter.takeBigSizePrefixedList(() =>
            iter.takeBigSizePrefixed(iter.take))

        ScriptWitness(stack.reverse) match {
          case EmptyScriptWitness =>
            throw new IllegalArgumentException(s"Invalid witness: $stack")
          case witness: ScriptWitnessV0 => witness
          case tw: TaprootWitness =>
            sys.error(s"Taproot witness is not supported by dlc spec, got=$tw")
        }
      }
      FundingSignaturesV0TLV(witnesses, DLCSerializationVersion.Gamma)
    }

    t.getOrElse(fromOldTLVValue(bytes))

  }
}

sealed trait DLCSetupTLV extends TLV {
  def protocolVersionOpt: Option[Int]
  def protocolVersionU32Opt: Option[UInt32] = protocolVersionOpt.map(UInt32(_))
}

case class SendOfferTLV(
    peer: NormalizedString,
    message: NormalizedString,
    offer: DLCOfferTLV)
    extends DLCSetupTLV {

  require(peer.length <= 1024, "peer length must not exceed 1024 characters")
  require(message.length <= 1024,
          "message length must not exceed 1024 characters")

  //need to come back and set this?
  override val protocolVersionOpt: Option[Int] = None

  override val tpe: BigSizeUInt = SendOfferTLV.tpe

  override val value: ByteVector = {
    strBytes(peer) ++ strBytes(message) ++ offer.bytes
  }
}

object SendOfferTLV extends TLVFactory[SendOfferTLV] {

  override val tpe: BigSizeUInt = BigSizeUInt(65534)

  override val typeName: String = "SendOfferTLV"

  override def fromTLVValue(value: ByteVector): SendOfferTLV = {
    val iter = ValueIterator(value)
    val peer = iter.takeString()
    val message = iter.takeString()
    val offer = iter.take(DLCOfferTLV)
    SendOfferTLV(peer, message, offer)
  }
}

case class DLCOfferTLV(
    protocolVersionOpt: Option[Int],
    contractFlags: Byte,
    chainHash: DoubleSha256Digest,
    //in version 2 of the DLC spec, we embed the tempContractId in the offer message
    //rather than hashing the serialized offer message
    tempContractIdOpt: Option[Sha256Digest],
    contractInfo: ContractInfoTLV,
    fundingPubKey: ECPublicKey,
    payoutSPK: ScriptPubKey,
    payoutSerialId: UInt64,
    offererCollateralSatoshis: Satoshis,
    fundingInputs: Vector[FundingInputTLV],
    changeSPK: ScriptPubKey,
    changeSerialId: UInt64,
    fundOutputSerialId: UInt64,
    feeRate: SatoshisPerVirtualByte,
    contractMaturityBound: BlockTimeStamp,
    contractTimeout: BlockTimeStamp)
    extends DLCSetupTLV {
  require(
    changeSerialId != fundOutputSerialId,
    s"changeSerialId ($changeSerialId) cannot be equal to fundOutputSerialId ($fundOutputSerialId)")

  if (protocolVersionOpt.isDefined && protocolVersionOpt.get == 1) {
    require(tempContractIdOpt.isDefined,
            s"TempContractId must be defined in version 1 of the protocol")
  } else {
    require(
      tempContractIdOpt.isEmpty,
      s"Cannot have tempContractId defined prior to version 1 of the protocol. tempContractId is the hash of the entire TLV prior to version 1"
    )
  }

  override val tpe: BigSizeUInt = DLCOfferTLV.tpe

  override val value: ByteVector = {
    val versionBytes = protocolVersionU32Opt match {
      case Some(v) => v.bytes
      case None    => ByteVector.empty
    }
    versionBytes ++
      ByteVector(contractFlags) ++
      chainHash.bytes ++ {
        if (protocolVersionOpt.isDefined) {
          //if we have have version 1 of the protocol we do NOT
          //serialize the tempContractId, if we have version 2
          //of the protocol we DO serialize the tempContractId

          tempContractIdOpt.map(_.bytes).get
        } else {
          ByteVector.empty
        }
      } ++
      contractInfo.bytes ++
      fundingPubKey.bytes ++
      TLV.encodeScript(payoutSPK) ++
      payoutSerialId.bytes ++
      satBytes(offererCollateralSatoshis) ++
      bigSizePrefixedList(fundingInputs) ++
      TLV.encodeScript(changeSPK) ++
      changeSerialId.bytes ++
      fundOutputSerialId.bytes ++
      satBytes(feeRate.currencyUnit.satoshis) ++
      contractMaturityBound.toUInt32.bytes ++
      contractTimeout.toUInt32.bytes
  }

  val tempContractId: Sha256Digest = {
    tempContractIdOpt match {
      case Some(tempContractId) => tempContractId
      case None                 => CryptoUtil.sha256(LnMessage(this).bytes)
    }
  }
}

object DLCOfferTLV extends TLVFactory[DLCOfferTLV] {

  /** No version for now */
  val currentVersionOpt: Option[Int] = Some(1)

  val currentVersionU32: Option[UInt32] = {
    currentVersionOpt.map(UInt32(_))
  }
  override val tpe: BigSizeUInt = BigSizeUInt(42778)

  override def fromTLVValue(value: ByteVector): DLCOfferTLV = {
    var iter = ValueIterator(value)

    val versionBytesOpt = iter.current.take(4)
    val protocolVersionOpt = {
      if (
        versionBytesOpt.take(1).toInt(false) == 0 && versionBytesOpt.toInt(
          false) == 1
      ) {
        //before the Gamma version of the protocol, we did not have a
        //version. The first byte was the 'contractFlags' and
        //in released software that was always set to zero.
        Some(iter.takeU32().toInt)
      } else {
        None
      }
    }

    val contractFlags = iter.take(1).head

    val chainHash = iter.take(DoubleSha256Digest, 32)
    val contractInfoT = Try(ContractInfoTLV.fromBytes(iter.current))
    val (contractInfo, tempContractIdOpt) = contractInfoT match {
      case Success(contractInfo) =>
        iter = ValueIterator(iter.current.drop(contractInfo.byteSize))
        (contractInfo, None)
      case Failure(_) =>
//        //means we are using the new updated DLC protocol which has
//        //the temporaryContractId serialized inside of the offer
//        //message. We need to parse that, and then parse the contractInfo
        val tempContractId = iter.take(Sha256Digest, 32)
        val contractInfo = ContractInfoTLV.fromBytes(iter.current)
        iter = ValueIterator(iter.current.drop(contractInfo.bytes.length))
        (contractInfo, Some(tempContractId))
    }
    val fundingPubKey = iter.take(ECPublicKey, 33)
    val payoutSPK = iter.takeSPK()
    val payoutSerialId = iter.takeU64()
    val totalCollateralSatoshis = iter.takeSats()

    val fundingInputs = {
      if (protocolVersionOpt.isEmpty) {
        iter.takeU16PrefixedList(() => iter.take(FundingInputV0TLV))
      } else {
        iter.takeBigSizePrefixedList(() => iter.take(FundingInputV0TLV))
      }
    }

    val changeSPK = iter.takeSPK()
    val changeSerialId = iter.takeU64()
    val fundingOutputSerialId = iter.takeU64()
    val feeRate = SatoshisPerVirtualByte(iter.takeSats())

    val contractMaturityBound = BlockTimeStamp(iter.takeU32())
    val contractTimeout = BlockTimeStamp(iter.takeU32())

    DLCOfferTLV(
      protocolVersionOpt = protocolVersionOpt,
      contractFlags = contractFlags,
      chainHash = chainHash,
      tempContractIdOpt = tempContractIdOpt,
      contractInfo = contractInfo,
      fundingPubKey = fundingPubKey,
      payoutSPK = payoutSPK,
      payoutSerialId = payoutSerialId,
      offererCollateralSatoshis = totalCollateralSatoshis,
      fundingInputs = fundingInputs,
      changeSPK = changeSPK,
      changeSerialId = changeSerialId,
      fundOutputSerialId = fundingOutputSerialId,
      feeRate = feeRate,
      contractMaturityBound = contractMaturityBound,
      contractTimeout = contractTimeout
    )
  }

  override val typeName: String = "DLCOfferTLV"
}

sealed trait NegotiationFieldsTLV extends DLCSubType

object NegotiationFieldsTLV extends Factory[NegotiationFieldsTLV] {

  final val empty: NoNegotiationFieldsTLV = NoNegotiationFieldsTLV(
    DLCSerializationVersion.current)

  val allFactories: Vector[DLCSubTypeFactory[NegotiationFieldsTLV]] = {
    Vector(NoNegotiationFieldsTLV, NegotiationFieldsV1TLV)
  }

  private val knownSubTypes: Vector[Byte] = {
    Vector(NoNegotiationFieldsTLV.subType, NegotiationFieldsV1TLV.subType)
  }

  private val subTypeMap: Map[Byte, DLCSubTypeFactory[NegotiationFieldsTLV]] = {
    knownSubTypes.zip(allFactories).toMap
  }

  private def fromOldTLVValue(bytes: ByteVector): NegotiationFieldsTLV = {
    Try(NoNegotiationFieldsTLV.fromBytes(bytes))
      .getOrElse(NegotiationFieldsV1TLV.fromBytes(bytes))
  }

  override def fromBytes(bytes: ByteVector): NegotiationFieldsTLV = {
    val t = Try {
      SubTypeUtil.fromBytes(bytes, subTypeMap)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }

}

case class NoNegotiationFieldsTLV(serializationVersion: DLCSerializationVersion)
    extends NegotiationFieldsTLV {
  override val subType: Byte = NoNegotiationFieldsTLV.subType

  override val bytes: ByteVector = ByteVector.fromByte(subType)

  override val byteSize: Long = {
    serializationVersion match {
      case DLCSerializationVersion.Alpha | DLCSerializationVersion.Beta =>
        val value =
          ByteVector.empty //no bytes when we have no negotiation fields
        val len: BigSizeUInt = BigSizeUInt.calcFor(value)
        FundingInputV0TLV.oldTpe.byteSize + len.byteSize + value.size
      case DLCSerializationVersion.Gamma =>
        super.byteSize
    }
  }
}

object NoNegotiationFieldsTLV
    extends DLCSubTypeFactory[NoNegotiationFieldsTLV] {
  val oldTpe: BigSizeUInt = BigSizeUInt(55334)

  val subType: Byte = 0

  private def fromOldTLVValue(bytes: ByteVector): NoNegotiationFieldsTLV = {
    val DecodeTLVResult(parsedTpe, _, _) = TLV.decodeTLV(bytes)

    require(
      parsedTpe == oldTpe,
      s"Got ${TLV.getTypeName(oldTpe)} type when expecting NoNegotiationFieldsTLV")

    NoNegotiationFieldsTLV(DLCSerializationVersion.Beta)
  }

  override def fromBytes(bytes: ByteVector): NoNegotiationFieldsTLV = {
    val t = Try {
      require(bytes.head == subType, "NoNegotiationsFieldsTLV must be empty")

      NoNegotiationFieldsTLV(DLCSerializationVersion.Gamma)
    }

    t.getOrElse(fromOldTLVValue(bytes))
  }
}

case class NegotiationFieldsV1TLV(
    roundingIntervalsV0TLV: RoundingIntervalsV0TLV)
    extends NegotiationFieldsTLV {
  override val subType: Byte = NegotiationFieldsV1TLV.subType

  override val bytes: ByteVector = {
    ByteVector.fromByte(
      NegotiationFieldsV1TLV.subType) ++ roundingIntervalsV0TLV.bytes
  }
}

object NegotiationFieldsV1TLV
    extends DLCSubTypeFactory[NegotiationFieldsV1TLV] {
  final val subType: Byte = 1

  override def fromBytes(bytes: ByteVector): NegotiationFieldsV1TLV = {
    require(bytes.head == subType,
            s"Expected subType=$subType got=${bytes.head}")

    val iter = ValueIterator(bytes.drop(1))

    val roundingIntervals = iter.take(RoundingIntervalsV0TLV)

    NegotiationFieldsV1TLV(roundingIntervals)
  }
}

case class NegotiationFieldsV2TLV(
    nestedNegotiationFields: Vector[NegotiationFieldsTLV])
    extends NegotiationFieldsTLV {
  require(
    nestedNegotiationFields.forall(!_.isInstanceOf[NegotiationFieldsV2TLV]))

  override val subType: Byte = NegotiationFieldsV2TLV.subType

  override val bytes: ByteVector = {
    ByteVector.fromByte(subType) ++ TLVUtil.bigSizePrefixedList(
      nestedNegotiationFields)
  }
}

object NegotiationFieldsV2TLV
    extends DLCSubTypeFactory[NegotiationFieldsV2TLV] {
  final val subType: Byte = 2.toByte

  override def fromBytes(value: ByteVector): NegotiationFieldsV2TLV = {
    val iter = ValueIterator(value)

    val nestedNegotiationFields =
      iter.takeBigSizePrefixedList(() => iter.take(NegotiationFieldsTLV))

    NegotiationFieldsV2TLV(nestedNegotiationFields)
  }
}

case class DLCAcceptTLV(
    protocolVersionOpt: Option[Int],
    tempContractId: Sha256Digest,
    acceptCollateralSatoshis: Satoshis,
    fundingPubKey: ECPublicKey,
    payoutSPK: ScriptPubKey,
    payoutSerialId: UInt64,
    fundingInputs: Vector[FundingInputTLV],
    changeSPK: ScriptPubKey,
    changeSerialId: UInt64,
    cetSignatures: CETSignaturesTLV,
    refundSignature: ECDigitalSignature,
    negotiationFields: NegotiationFieldsTLV)
    extends DLCSetupTLV {
  override val tpe: BigSizeUInt = DLCAcceptTLV.tpe

  override val value: ByteVector = {
    val negotiationFieldsBytes = negotiationFields.bytes
    val versionBytes = protocolVersionU32Opt match {
      case Some(v) => v.bytes
      case None    => ByteVector.empty
    }
    versionBytes ++
      tempContractId.bytes ++
      satBytes(acceptCollateralSatoshis) ++
      fundingPubKey.bytes ++
      TLV.encodeScript(payoutSPK) ++
      payoutSerialId.bytes ++
      bigSizePrefixedList(fundingInputs) ++
      TLV.encodeScript(changeSPK) ++
      changeSerialId.bytes ++
      cetSignatures.bytes ++
      refundSignature.toRawRS ++
      negotiationFieldsBytes
  }

  val refundPartialSignature: PartialSignature = {
    PartialSignature(fundingPubKey, refundSignature)
  }
}

object DLCAcceptTLV extends TLVFactory[DLCAcceptTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42780)

  override def fromTLVValue(value: ByteVector): DLCAcceptTLV = {
    val iter = ValueIterator(value)
    val versionBytes = iter.current.take(4)
    val protocolVersionOpt = if (versionBytes.toInt(false) == 1) {
      Some(iter.takeU32().toInt)
    } else {
      None
    }
    val tempContractId = iter.take(Sha256Digest, 32)
    val totalCollateralSatoshis = iter.takeSats()
    val fundingPubKey = iter.take(ECPublicKey, 33)
    val payoutSPK = iter.takeSPK()
    val payoutSerialId = iter.takeU64()

    val fundingInputs = {
      if (protocolVersionOpt.isEmpty) {
        iter.takeU16PrefixedList(() => iter.take(FundingInputV0TLV))
      } else {
        iter.takeBigSizePrefixedList(() => iter.take(FundingInputV0TLV))
      }
    }

    val changeSPK = iter.takeSPK()

    val changeSerialId = iter.takeU64()

    val cetSignatures = iter.take(CETSignaturesV0TLV)

    val refundSignature = ECDigitalSignature.fromRS(iter.take(64))
    val negotiationFields = iter.take(NegotiationFieldsTLV)

    DLCAcceptTLV(
      protocolVersionOpt,
      tempContractId,
      totalCollateralSatoshis,
      fundingPubKey,
      payoutSPK,
      payoutSerialId,
      fundingInputs,
      changeSPK,
      changeSerialId,
      cetSignatures,
      refundSignature,
      negotiationFields
    )
  }

  override val typeName: String = "DLCAcceptTLV"
}

case class DLCSignTLV(
    protocolVersionOpt: Option[Int],
    contractId: ByteVector,
    cetSignatures: CETSignaturesTLV,
    refundSignature: ECDigitalSignature,
    fundingSignatures: FundingSignaturesTLV)
    extends DLCSetupTLV {
  override val tpe: BigSizeUInt = DLCSignTLV.tpe

  override val value: ByteVector = {
    val versionBytes = protocolVersionU32Opt match {
      case Some(v) => v.bytes
      case None    => ByteVector.empty
    }
    versionBytes ++ contractId ++
      cetSignatures.bytes ++
      refundSignature.toRawRS ++
      fundingSignatures.bytes
  }

  def getPartialSignature(fundingPubKey: ECPublicKey): PartialSignature = {
    PartialSignature(fundingPubKey, refundSignature)
  }
}

object DLCSignTLV extends TLVFactory[DLCSignTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42782)

  override def fromTLVValue(value: ByteVector): DLCSignTLV = {
    val iter = ValueIterator(value)
    val versionBytes = iter.current.take(4)
    val protocolVersionOpt = if (versionBytes.toInt(false) == 1) {
      Some(iter.takeU32().toInt)
    } else {
      None
    }
    val contractId = iter.take(32)
    val cetSignatures = iter.take(CETSignaturesV0TLV)
    val refundSignature = ECDigitalSignature.fromRS(iter.take(64))
    val fundingSignatures = iter.take(FundingSignaturesV0TLV)

    DLCSignTLV(protocolVersionOpt,
               contractId,
               cetSignatures,
               refundSignature,
               fundingSignatures)
  }

  override val typeName: String = "DLCSignTLV"
}
