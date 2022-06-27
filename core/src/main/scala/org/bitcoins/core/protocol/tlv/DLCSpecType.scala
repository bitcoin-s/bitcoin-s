package org.bitcoins.core.protocol.tlv

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{UInt16, UInt32, UInt64}
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.dlc.models.{
  DLCPayoutCurve,
  PiecewisePolynomialEndpoint
}
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  EmptyScriptSignature,
  EmptyScriptWitness,
  NonWitnessScriptPubKey,
  P2SHScriptSignature,
  ScriptWitness,
  ScriptWitnessV0,
  TaprootWitness,
  WitnessScriptPubKey
}
import org.bitcoins.core.protocol.tlv.TLV.DecodeTLVResult
import org.bitcoins.core.protocol.transaction.{
  EmptyTransaction,
  OutputReference,
  Transaction,
  TransactionInput,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.util.sorted.OrderedAnnouncements
import org.bitcoins.crypto.{
  CryptoUtil,
  ECAdaptorSignature,
  Factory,
  NetworkElement
}
import scodec.bits.ByteVector

import scala.util.Try

sealed trait DLCSpecType extends NetworkElement

sealed trait DLCPlainType extends DLCSpecType

sealed trait DLCSubType extends DLCSpecType {
  def subType: Byte
}

sealed trait DLCSubTypeFactory[+T <: DLCSubType] extends Factory[T]

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
    oracleParamsOpt: OptionDLCType[OracleParamsV0TLV],
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
        OracleInfoV1TLV.oldTpe.byteSize + BigSizeUInt
          .calcFor(value)
          .byteSize + value.size
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
                    NoneDLCType,
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
        ContractInfoV0TLV.oldTpe.byteSize + BigSizeUInt(
          valueByteSize).byteSize + valueByteSize
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
