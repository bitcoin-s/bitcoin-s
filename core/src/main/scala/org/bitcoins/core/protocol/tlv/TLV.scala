package org.bitcoins.core.protocol.tlv

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{UInt16, UInt32, UInt64}
import org.bitcoins.core.protocol.{BigSizeUInt, BlockTimeStamp}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.tlv.TLV.DecodeTLVResult
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  TransactionOutPoint
}
import org.bitcoins.core.wallet.fee.SatoshisPerKW
import org.bitcoins.crypto.{
  ECAdaptorSignature,
  ECDigitalSignature,
  ECPublicKey,
  Factory,
  NetworkElement,
  SchnorrNonce,
  SchnorrPublicKey,
  Sha256Digest,
  Sha256DigestBE
}
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
    Vector(
      ErrorTLV,
      PingTLV,
      PongTLV,
      ContractInfoV0TLV,
      OracleInfoV0TLV,
      FundingInputTempTLV,
      CETSignaturesV0TLV,
      FundingSignaturesV0TLV,
      DLCOfferTLV,
      DLCAcceptTLV,
      DLCSignTLV
    )

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

sealed trait ContractInfoTLV extends TLV

case class ContractInfoV0TLV(outcomes: Map[Sha256Digest, Satoshis])
    extends ContractInfoTLV {
  override val tpe: BigSizeUInt = ContractInfoV0TLV.tpe

  override val value: ByteVector = {
    outcomes.foldLeft(ByteVector.empty) {
      case (bytes, (outcome, amt)) =>
        bytes ++ outcome.bytes ++ amt.toUInt64.bytes
    }
  }
}

object ContractInfoV0TLV extends TLVFactory[ContractInfoV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42768)

  override def fromTLVValue(value: ByteVector): ContractInfoV0TLV = {
    val iter = ValueIterator(value)

    val builder = Map.newBuilder[Sha256Digest, Satoshis]

    while (iter.index < value.length) {
      val outcome = Sha256Digest(iter.take(32))
      val amt = Satoshis(UInt64(iter.takeBits(64)))
      builder.+=(outcome -> amt)
    }

    ContractInfoV0TLV(builder.result())
  }
}

sealed trait OracleInfoTLV extends TLV

case class OracleInfoV0TLV(pubKey: SchnorrPublicKey, rValue: SchnorrNonce)
    extends OracleInfoTLV {
  override def tpe: BigSizeUInt = OracleInfoV0TLV.tpe

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

sealed trait FundingInputTLV extends TLV

case class FundingInputTempTLV(outputRef: OutputReference)
    extends FundingInputTLV {
  override val tpe: BigSizeUInt = FundingInputTempTLV.tpe

  override val value: ByteVector = outputRef.bytes
}

object FundingInputTempTLV extends TLVFactory[FundingInputTempTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42772)

  override def fromTLVValue(value: ByteVector): FundingInputTempTLV = {
    FundingInputTempTLV(OutputReference(value))
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

case class FundingSignaturesV0TLV(
    sigs: Map[TransactionOutPoint, ECDigitalSignature])
    extends FundingSignaturesTLV {
  override val tpe: BigSizeUInt = FundingSignaturesV0TLV.tpe

  override val value: ByteVector = {
    sigs.foldLeft(ByteVector.empty) {
      case (bytes, (outPoint, sig)) =>
        bytes ++ outPoint.bytes ++ sig.bytes
    }
  }
}

object FundingSignaturesV0TLV extends TLVFactory[FundingSignaturesV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42776)

  override def fromTLVValue(value: ByteVector): FundingSignaturesV0TLV = {
    val iter = ValueIterator(value)

    val builder = Map.newBuilder[TransactionOutPoint, ECDigitalSignature]

    while (iter.index < value.length) {
      val outPoint = TransactionOutPoint(iter.take(36))
      val sig = ECDigitalSignature.fromFrontOfBytes(iter.current)
      iter.skip(sig)
      builder.+=(outPoint -> sig)
    }

    FundingSignaturesV0TLV(builder.result())
  }
}

case class DLCOfferTLV(
    contractFlags: Byte,
    chainHash: Sha256DigestBE,
    contractInfo: ContractInfoTLV,
    oracleInfo: OracleInfoTLV,
    fundingPubKey: ECPublicKey,
    payoutSPK: ScriptPubKey,
    totalCollateralSatoshis: Satoshis,
    fundingInputs: Vector[FundingInputTLV],
    changeSPK: ScriptPubKey,
    feeRatePerKW: SatoshisPerKW,
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
      payoutSPK.bytes ++
      totalCollateralSatoshis.toUInt64.bytes ++
      UInt16(fundingInputs.length).bytes ++
      fundingInputs.foldLeft(ByteVector.empty)(_ ++ _.bytes) ++
      changeSPK.bytes ++
      feeRatePerKW.currencyUnit.satoshis.toUInt64.bytes ++
      contractMaturityBound.toUInt32.bytes ++
      contractTimeout.toUInt32.bytes
  }
}

object DLCOfferTLV extends TLVFactory[DLCOfferTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42778)

  override def fromTLVValue(value: ByteVector): DLCOfferTLV = {
    val iter = ValueIterator(value)

    val contractFlags = iter.take(1).head
    val chainHash = Sha256DigestBE(iter.take(32))
    val contractInfo = ContractInfoV0TLV.fromBytes(iter.current)
    iter.skip(contractInfo)
    val oracleInfo = OracleInfoV0TLV.fromBytes(iter.current)
    iter.skip(oracleInfo)
    val fundingPubKey = ECPublicKey(iter.take(33))
    val payoutSPK = ScriptPubKey(iter.current)
    iter.skip(payoutSPK)
    val totalCollateralSatoshis = Satoshis(UInt64(iter.takeBits(64)))
    val numFundingInputs = UInt16(iter.takeBits(16))
    val fundingInputs = (0 until numFundingInputs.toInt).toVector.map { _ =>
      val fundingInput = FundingInputTempTLV.fromBytes(iter.current)
      iter.skip(fundingInput)
      fundingInput
    }
    val changeSPK = ScriptPubKey(iter.current)
    iter.skip(changeSPK)
    val feeRatePerKW = SatoshisPerKW(Satoshis(UInt64(iter.takeBits(64))))
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
      feeRatePerKW,
      contractMaturityBound,
      contractTimeout
    )
  }
}

case class DLCAcceptTLV(
    tempContractId: Sha256DigestBE,
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
      payoutSPK.bytes ++
      UInt16(fundingInputs.length).bytes ++
      fundingInputs.foldLeft(ByteVector.empty)(_ ++ _.bytes) ++
      changeSPK.bytes ++
      cetSignatures.bytes ++
      refundSignature.bytes
  }
}

object DLCAcceptTLV extends TLVFactory[DLCAcceptTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42780)

  override def fromTLVValue(value: ByteVector): DLCAcceptTLV = {
    val iter = ValueIterator(value)

    val tempContractId = Sha256DigestBE(iter.take(32))
    val totalCollateralSatoshis = Satoshis(UInt64(iter.takeBits(64)))
    val fundingPubKey = ECPublicKey(iter.take(33))
    val payoutSPK = ScriptPubKey(iter.current)
    iter.skip(payoutSPK)
    val numFundingInputs = UInt16(iter.takeBits(16))
    val fundingInputs = (0 until numFundingInputs.toInt).toVector.map { _ =>
      val fundingInput = FundingInputTempTLV.fromBytes(iter.current)
      iter.skip(fundingInput)
      fundingInput
    }
    val changeSPK = ScriptPubKey(iter.current)
    iter.skip(changeSPK)
    val cetSignatures = CETSignaturesV0TLV.fromBytes(iter.current)
    iter.skip(cetSignatures)
    val refundSignature = ECDigitalSignature(iter.current)
    iter.skip(refundSignature)

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
    contractId: Sha256DigestBE,
    cetSignatures: CETSignaturesTLV,
    refundSignature: ECDigitalSignature,
    fundingSignatures: FundingSignaturesTLV)
    extends TLV {
  override val tpe: BigSizeUInt = DLCSignTLV.tpe

  override val value: ByteVector = {
    contractId.bytes ++
      cetSignatures.bytes ++
      refundSignature.bytes ++
      fundingSignatures.bytes
  }
}

object DLCSignTLV extends TLVFactory[DLCSignTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42782)

  override def fromTLVValue(value: ByteVector): DLCSignTLV = {
    val iter = ValueIterator(value)

    val contractId = Sha256DigestBE(iter.take(32))
    val cetSignatures = CETSignaturesV0TLV.fromBytes(iter.current)
    iter.skip(cetSignatures)
    val refundSignature = ECDigitalSignature.fromFrontOfBytes(iter.current)
    iter.skip(refundSignature)
    val fundingSignatures = FundingSignaturesV0TLV.fromBytes(iter.current)
    iter.skip(fundingSignatures)

    DLCSignTLV(contractId, cetSignatures, refundSignature, fundingSignatures)
  }
}
