package org.bitcoins.core.protocol.tlv

import java.nio.charset.StandardCharsets

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv.TLV.DecodeTLVResult
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BigSizeUInt, BlockTimeStamp}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
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

  val allFactories: Vector[TLVFactory[TLV]] = {
    Vector(
      ErrorTLV,
      PingTLV,
      PongTLV,
      OracleEventV0TLV,
      OracleAnnouncementV0TLV,
      FundingInputV0TLV,
      CETSignaturesV0TLV,
      FundingSignaturesV0TLV,
      DLCOfferTLV,
      DLCAcceptTLV,
      DLCSignTLV
    ) ++ EventDescriptorTLV.allFactories ++
      ContractInfoTLV.allFactories ++
      OracleInfoTLV.allFactories
  }

  // Need to override to be able to default to Unknown
  override def fromBytes(bytes: ByteVector): TLV = {
    val DecodeTLVResult(tpe, _, value) = decodeTLV(bytes)

    allFactories.find(_.tpe == tpe) match {
      case Some(tlvFactory) => tlvFactory.fromTLVValue(value)
      case None             => UnknownTLV(tpe, value)
    }
  }

  def encodeScript(script: Script): ByteVector = {
    UInt16(script.asmBytes.length).bytes ++ script.asmBytes
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

    def takePoint(): TLVPoint = {
      val point = TLVPoint(current)
      skip(point)
      point
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

object EventDescriptorTLV extends TLVParentFactory[EventDescriptorTLV] {

  val allFactories: Vector[TLVFactory[EventDescriptorTLV]] =
    Vector(ExternalEventDescriptorV0TLV,
           EnumEventDescriptorV0TLV,
           RangeEventDescriptorV0TLV)

  override def typeName: String = "EventDescriptorTLV"
}

case class ExternalEventDescriptorV0TLV(external_name: String)
    extends EventDescriptorTLV {
  override def tpe: BigSizeUInt = ExternalEventDescriptorV0TLV.tpe

  override val value: ByteVector = CryptoUtil.serializeForHash(external_name)
}

object ExternalEventDescriptorV0TLV
    extends TLVFactory[ExternalEventDescriptorV0TLV] {

  override def apply(external_name: String): ExternalEventDescriptorV0TLV =
    new ExternalEventDescriptorV0TLV(external_name)

  override val tpe: BigSizeUInt = BigSizeUInt(55300)

  override def fromTLVValue(value: ByteVector): ExternalEventDescriptorV0TLV = {
    val external_name = new String(value.toArray, StandardCharsets.UTF_8)

    ExternalEventDescriptorV0TLV(external_name)
  }
}

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

case class RangeEventDescriptorV0TLV(start: Int32, stop: Int32, step: UInt16)
    extends EventDescriptorTLV {
  override def tpe: BigSizeUInt = RangeEventDescriptorV0TLV.tpe

  override val value: ByteVector = {
    start.bytes ++ stop.bytes ++ step.bytes
  }
}

object RangeEventDescriptorV0TLV extends TLVFactory[RangeEventDescriptorV0TLV] {

  override val tpe: BigSizeUInt = BigSizeUInt(55304)

  override def fromTLVValue(value: ByteVector): RangeEventDescriptorV0TLV = {
    val iter = ValueIterator(value)

    val start = Int32(iter.takeBits(32))
    val stop = Int32(iter.takeBits(32))
    val step = UInt16(iter.takeBits(16))

    RangeEventDescriptorV0TLV(start, stop, step)
  }
}

sealed trait OracleEventTLV extends TLV

case class OracleEventV0TLV(
    publicKey: SchnorrPublicKey,
    nonce: SchnorrNonce,
    eventMaturityEpoch: UInt32,
    eventDescriptor: EventDescriptorTLV,
    eventURI: String
) extends OracleEventTLV {
  override def tpe: BigSizeUInt = OracleEventV0TLV.tpe

  override val value: ByteVector = {
    val uriBytes = CryptoUtil.serializeForHash(eventURI)
    publicKey.bytes ++ nonce.bytes ++ eventMaturityEpoch.bytes ++ eventDescriptor.bytes ++ uriBytes
  }
}

object OracleEventV0TLV extends TLVFactory[OracleEventV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(55330)

  override def fromTLVValue(value: ByteVector): OracleEventV0TLV = {
    val iter = ValueIterator(value)

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
    announcementSignature: SchnorrDigitalSignature,
    eventTLV: OracleEventV0TLV)
    extends OracleAnnouncementTLV {
  override def tpe: BigSizeUInt = OracleAnnouncementV0TLV.tpe

  override val value: ByteVector = announcementSignature.bytes ++ eventTLV.bytes
}

object OracleAnnouncementV0TLV extends TLVFactory[OracleAnnouncementV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(55332)

  override def fromTLVValue(value: ByteVector): OracleAnnouncementV0TLV = {
    val iter = ValueIterator(value)

    val sig = SchnorrDigitalSignature(iter.take(64))
    val eventTLV = OracleEventV0TLV(iter.current)

    OracleAnnouncementV0TLV(sig, eventTLV)

  }
}

sealed trait ContractInfoTLV extends TLV

object ContractInfoTLV extends TLVParentFactory[ContractInfoTLV] {

  val allFactories: Vector[TLVFactory[ContractInfoTLV]] =
    Vector(ContractInfoV0TLV, ContractInfoV1TLV)

  override def typeName: String = "ContractInfoTLV"
}

case class ContractInfoV0TLV(outcomes: Vector[(String, Satoshis)])
    extends ContractInfoTLV {
  override val tpe: BigSizeUInt = ContractInfoV0TLV.tpe

  override val value: ByteVector = {
    outcomes.foldLeft(ByteVector.empty) {
      case (bytes, (outcome, amt)) =>
        val outcomeBytes = CryptoUtil.serializeForHash(outcome)
        bytes ++ BigSizeUInt
          .calcFor(outcomeBytes)
          .bytes ++ outcomeBytes ++ amt.toUInt64.bytes
    }
  }
}

object ContractInfoV0TLV extends TLVFactory[ContractInfoV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42768)

  override def fromTLVValue(value: ByteVector): ContractInfoV0TLV = {
    val iter = ValueIterator(value)

    val builder = Vector.newBuilder[(String, Satoshis)]

    while (iter.index < value.length) {
      val outcomeLen = BigSizeUInt(iter.current)
      iter.skip(outcomeLen)
      val outcome =
        new String(iter.take(outcomeLen.toInt).toArray, StandardCharsets.UTF_8)
      val amt = Satoshis(UInt64(iter.takeBits(64)))
      builder.+=(outcome -> amt)
    }

    ContractInfoV0TLV(builder.result())
  }
}

case class TLVPoint(outcome: Long, value: Satoshis, isEndpoint: Boolean)
    extends NetworkElement {

  override def bytes: ByteVector = {
    val leadingByte = if (isEndpoint) {
      1.toByte
    } else {
      0.toByte
    }

    ByteVector(leadingByte) ++ BigSizeUInt(outcome).bytes ++ UInt64(
      value.toLong).bytes
  }
}

object TLVPoint extends Factory[TLVPoint] {

  override def fromBytes(bytes: ByteVector): TLVPoint = {
    val isEndpoint = bytes.head match {
      case 0 => false
      case 1 => true
      case b: Byte =>
        throw new IllegalArgumentException(
          s"Did not recognize leading byte: $b")
    }

    val outcome = BigSizeUInt(bytes.tail)
    val value = UInt64(bytes.drop(1 + outcome.byteSize).take(8))
    TLVPoint(outcome.toLong, Satoshis(value.toLong), isEndpoint)
  }
}

case class ContractInfoV1TLV(
    base: Int,
    numDigits: Int,
    totalCollateral: Satoshis,
    points: Vector[TLVPoint])
    extends ContractInfoTLV {
  override val tpe: BigSizeUInt = ContractInfoV1TLV.tpe

  override val value: ByteVector = {
    BigSizeUInt(base).bytes ++ UInt16(numDigits).bytes ++ UInt64(
      totalCollateral.toLong).bytes ++ BigSizeUInt(
      points.length).bytes ++ points.foldLeft(ByteVector.empty)(_ ++ _.bytes)
  }
}

object ContractInfoV1TLV extends TLVFactory[ContractInfoV1TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42784)

  override def fromTLVValue(value: ByteVector): ContractInfoV1TLV = {
    val iter = ValueIterator(value)

    val base = BigSizeUInt(iter.current)
    iter.skip(base)
    val numDigits = UInt16(iter.takeBits(16))
    val totalCollateral = UInt64(iter.takeBits(64))
    val numPoints = BigSizeUInt(iter.current)
    iter.skip(numPoints)
    val points = (0L until numPoints.toLong).toVector.map { _ =>
      iter.takePoint()
    }

    ContractInfoV1TLV(base.toInt,
                      numDigits.toInt,
                      Satoshis(totalCollateral.toLong),
                      points)
  }
}

sealed trait OracleInfoTLV extends TLV

object OracleInfoTLV extends TLVParentFactory[OracleInfoTLV] {

  override val allFactories: Vector[TLVFactory[OracleInfoTLV]] =
    Vector(OracleInfoV0TLV, OracleInfoV1TLV)

  override def typeName: String = "OracleInfoTLV"
}

case class OracleInfoV0TLV(pubKey: SchnorrPublicKey, rValue: SchnorrNonce)
    extends OracleInfoTLV {
  override val tpe: BigSizeUInt = OracleInfoV0TLV.tpe

  override val value: ByteVector = {
    pubKey.bytes ++ rValue.bytes
  }
}

object OracleInfoV0TLV extends TLVFactory[OracleInfoV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42770)

  override def fromTLVValue(value: ByteVector): OracleInfoV0TLV = {
    val (pubKeyBytes, rBytes) = value.splitAt(32)
    val pubKey = SchnorrPublicKey(pubKeyBytes)
    val rValue = SchnorrNonce(rBytes)

    OracleInfoV0TLV(pubKey, rValue)
  }
}

case class OracleInfoV1TLV(
    pubKey: SchnorrPublicKey,
    nonces: Vector[SchnorrNonce])
    extends OracleInfoTLV {
  override val tpe: BigSizeUInt = OracleInfoV1TLV.tpe

  override val value: ByteVector = {
    nonces.foldLeft(pubKey.bytes)(_ ++ _.bytes)
  }
}

object OracleInfoV1TLV extends TLVFactory[OracleInfoV1TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42786)

  override def fromTLVValue(value: ByteVector): OracleInfoV1TLV = {
    val iter = ValueIterator(value)

    val pubKey = SchnorrPublicKey(iter.take(32))
    val nonces = (0L until iter.current.length / 32).toVector.map { _ =>
      SchnorrNonce(iter.take(32))
    }

    OracleInfoV1TLV(pubKey, nonces)
  }
}

sealed trait FundingInputTLV extends TLV

case class FundingInputV0TLV(
    prevTx: Transaction,
    prevTxVout: UInt32,
    sequence: UInt32,
    maxWitnessLen: UInt16,
    redeemScriptOpt: Option[WitnessScriptPubKey])
    extends FundingInputTLV {
  override val tpe: BigSizeUInt = FundingInputV0TLV.tpe

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

  override val value: ByteVector = {
    val redeemScript =
      redeemScriptOpt.getOrElse(EmptyScriptPubKey)

    UInt16(prevTx.byteSize).bytes ++
      prevTx.bytes ++
      prevTxVout.bytes ++
      sequence.bytes ++
      maxWitnessLen.bytes ++
      TLV.encodeScript(redeemScript)
  }
}

object FundingInputV0TLV extends TLVFactory[FundingInputV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42772)

  override def fromTLVValue(value: ByteVector): FundingInputV0TLV = {
    val iter = ValueIterator(value)

    val prevTxLen = UInt16(iter.takeBits(16))
    val prevTx = Transaction(iter.take(prevTxLen.toInt))
    val prevTxVout = UInt32(iter.takeBits(32))
    val sequence = UInt32(iter.takeBits(32))
    val maxWitnessLen = UInt16(iter.takeBits(16))
    val redeemScript = iter.takeSPK()
    val redeemScriptOpt = redeemScript match {
      case EmptyScriptPubKey         => None
      case wspk: WitnessScriptPubKey => Some(wspk)
      case _: NonWitnessScriptPubKey =>
        throw new IllegalArgumentException(
          s"Redeem Script must be Segwith SPK: $redeemScript")
    }

    FundingInputV0TLV(prevTx,
                      prevTxVout,
                      sequence,
                      maxWitnessLen,
                      redeemScriptOpt)
  }
}

sealed trait CETSignaturesTLV extends TLV

case class CETSignaturesV0TLV(sigs: Vector[ECAdaptorSignature])
    extends CETSignaturesTLV {
  override val tpe: BigSizeUInt = CETSignaturesV0TLV.tpe

  override val value: ByteVector = {
    sigs.foldLeft(ByteVector.empty)(_ ++ _.bytes)
  }
}

object CETSignaturesV0TLV extends TLVFactory[CETSignaturesV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42774)

  override def fromTLVValue(value: ByteVector): CETSignaturesV0TLV = {
    val iter = ValueIterator(value)

    val builder = Vector.newBuilder[ECAdaptorSignature]

    while (iter.index < value.length) {
      val sig = ECAdaptorSignature(iter.take(162))
      builder.+=(sig)
    }

    CETSignaturesV0TLV(builder.result())
  }
}

sealed trait FundingSignaturesTLV extends TLV

case class FundingSignaturesV0TLV(witnesses: Vector[ScriptWitnessV0])
    extends FundingSignaturesTLV {
  override val tpe: BigSizeUInt = FundingSignaturesV0TLV.tpe

  override val value: ByteVector = {
    witnesses.foldLeft(UInt16(witnesses.length).bytes) {
      case (bytes, witness) =>
        witness.stack.reverse.foldLeft(
          bytes ++ UInt16(witness.stack.length).bytes) {
          case (bytes, stackElem) =>
            bytes ++ UInt16(stackElem.length).bytes ++ stackElem
        }
    }
  }
}

object FundingSignaturesV0TLV extends TLVFactory[FundingSignaturesV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42776)

  override def fromTLVValue(value: ByteVector): FundingSignaturesV0TLV = {
    val iter = ValueIterator(value)

    val numWitnesses = UInt16(iter.takeBits(16))
    val witnesses = (0 until numWitnesses.toInt).toVector.map { _ =>
      val numStackElements = UInt16(iter.takeBits(16))
      val stack = (0 until numStackElements.toInt).toVector.map { _ =>
        val stackElemLength = UInt16(iter.takeBits(16))
        iter.take(stackElemLength.toInt)
      }
      ScriptWitness(stack.reverse) match {
        case EmptyScriptWitness =>
          throw new IllegalArgumentException(s"Invalid witness: $stack")
        case witness: ScriptWitnessV0 => witness
      }
    }

    FundingSignaturesV0TLV(witnesses)
  }
}

case class DLCOfferTLV(
    contractFlags: Byte,
    chainHash: DoubleSha256Digest,
    contractInfo: ContractInfoTLV,
    oracleInfo: OracleInfoTLV,
    fundingPubKey: ECPublicKey,
    payoutSPK: ScriptPubKey,
    totalCollateralSatoshis: Satoshis,
    fundingInputs: Vector[FundingInputTLV],
    changeSPK: ScriptPubKey,
    feeRate: SatoshisPerVirtualByte,
    contractMaturityBound: BlockTimeStamp,
    contractTimeout: BlockTimeStamp)
    extends TLV {
  override val tpe: BigSizeUInt = DLCOfferTLV.tpe

  override val value: ByteVector = {
    ByteVector(contractFlags) ++
      chainHash.bytes ++
      contractInfo.bytes ++
      oracleInfo.bytes ++
      fundingPubKey.bytes ++
      TLV.encodeScript(payoutSPK) ++
      totalCollateralSatoshis.toUInt64.bytes ++
      UInt16(fundingInputs.length).bytes ++
      fundingInputs.foldLeft(ByteVector.empty)(_ ++ _.bytes) ++
      TLV.encodeScript(changeSPK) ++
      feeRate.currencyUnit.satoshis.toUInt64.bytes ++
      contractMaturityBound.toUInt32.bytes ++
      contractTimeout.toUInt32.bytes
  }
}

object DLCOfferTLV extends TLVFactory[DLCOfferTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42778)

  override def fromTLVValue(value: ByteVector): DLCOfferTLV = {
    val iter = ValueIterator(value)

    val contractFlags = iter.take(1).head
    val chainHash = DoubleSha256Digest(iter.take(32))
    val contractInfo = ContractInfoTLV.fromBytes(iter.current)
    iter.skip(contractInfo)
    val oracleInfo = OracleInfoTLV.fromBytes(iter.current)
    iter.skip(oracleInfo)
    val fundingPubKey = ECPublicKey(iter.take(33))
    val payoutSPK = iter.takeSPK()
    val totalCollateralSatoshis = Satoshis(UInt64(iter.takeBits(64)))
    val numFundingInputs = UInt16(iter.takeBits(16))
    val fundingInputs = (0 until numFundingInputs.toInt).toVector.map { _ =>
      val fundingInput = FundingInputV0TLV.fromBytes(iter.current)
      iter.skip(fundingInput)
      fundingInput
    }
    val changeSPK = iter.takeSPK()
    val feeRate = SatoshisPerVirtualByte(Satoshis(UInt64(iter.takeBits(64))))
    val contractMaturityBound = BlockTimeStamp(UInt32(iter.takeBits(32)))
    val contractTimeout = BlockTimeStamp(UInt32(iter.takeBits(32)))

    DLCOfferTLV(
      contractFlags,
      chainHash,
      contractInfo,
      oracleInfo,
      fundingPubKey,
      payoutSPK,
      totalCollateralSatoshis,
      fundingInputs,
      changeSPK,
      feeRate,
      contractMaturityBound,
      contractTimeout
    )
  }
}

case class DLCAcceptTLV(
    tempContractId: Sha256Digest,
    totalCollateralSatoshis: Satoshis,
    fundingPubKey: ECPublicKey,
    payoutSPK: ScriptPubKey,
    fundingInputs: Vector[FundingInputTLV],
    changeSPK: ScriptPubKey,
    cetSignatures: CETSignaturesTLV,
    refundSignature: ECDigitalSignature)
    extends TLV {
  override val tpe: BigSizeUInt = DLCAcceptTLV.tpe

  override val value: ByteVector = {
    tempContractId.bytes ++
      totalCollateralSatoshis.toUInt64.bytes ++
      fundingPubKey.bytes ++
      TLV.encodeScript(payoutSPK) ++
      UInt16(fundingInputs.length).bytes ++
      fundingInputs.foldLeft(ByteVector.empty)(_ ++ _.bytes) ++
      TLV.encodeScript(changeSPK) ++
      cetSignatures.bytes ++
      refundSignature.toRawRS
  }
}

object DLCAcceptTLV extends TLVFactory[DLCAcceptTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42780)

  override def fromTLVValue(value: ByteVector): DLCAcceptTLV = {
    val iter = ValueIterator(value)

    val tempContractId = Sha256Digest(iter.take(32))
    val totalCollateralSatoshis = Satoshis(UInt64(iter.takeBits(64)))
    val fundingPubKey = ECPublicKey(iter.take(33))
    val payoutSPK = iter.takeSPK()
    val numFundingInputs = UInt16(iter.takeBits(16))
    val fundingInputs = (0 until numFundingInputs.toInt).toVector.map { _ =>
      val fundingInput = FundingInputV0TLV.fromBytes(iter.current)
      iter.skip(fundingInput)
      fundingInput
    }
    val changeSPK = iter.takeSPK()
    val cetSignatures = CETSignaturesV0TLV.fromBytes(iter.current)
    iter.skip(cetSignatures)
    val refundSignature = ECDigitalSignature.fromRS(iter.take(64))

    DLCAcceptTLV(tempContractId,
                 totalCollateralSatoshis,
                 fundingPubKey,
                 payoutSPK,
                 fundingInputs,
                 changeSPK,
                 cetSignatures,
                 refundSignature)
  }
}

case class DLCSignTLV(
    contractId: ByteVector,
    cetSignatures: CETSignaturesTLV,
    refundSignature: ECDigitalSignature,
    fundingSignatures: FundingSignaturesTLV)
    extends TLV {
  override val tpe: BigSizeUInt = DLCSignTLV.tpe

  override val value: ByteVector = {
    contractId ++
      cetSignatures.bytes ++
      refundSignature.toRawRS ++
      fundingSignatures.bytes
  }
}

object DLCSignTLV extends TLVFactory[DLCSignTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42782)

  override def fromTLVValue(value: ByteVector): DLCSignTLV = {
    val iter = ValueIterator(value)

    val contractId = iter.take(32)
    val cetSignatures = CETSignaturesV0TLV.fromBytes(iter.current)
    iter.skip(cetSignatures)
    val refundSignature = ECDigitalSignature.fromRS(iter.take(64))
    val fundingSignatures = FundingSignaturesV0TLV.fromBytes(iter.current)
    iter.skip(fundingSignatures)

    DLCSignTLV(contractId, cetSignatures, refundSignature, fundingSignatures)
  }
}
