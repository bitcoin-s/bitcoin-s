package org.bitcoins.core.protocol.tlv

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{UInt16, UInt32, UInt64}
import org.bitcoins.core.protocol.{BigSizeUInt, BlockTimeStamp}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.tlv.TLV.DecodeTLVResult
import org.bitcoins.core.protocol.transaction.OutputReference
import org.bitcoins.core.wallet.fee.SatoshisPerKW
import org.bitcoins.crypto.{
  ECPublicKey,
  Factory,
  NetworkElement,
  SchnorrNonce,
  SchnorrPublicKey,
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
    Vector(ErrorTLV,
           PingTLV,
           PongTLV,
           ContractInfoV0TLV,
           OracleInfoV0TLV,
           FundingInputTempTLV,
           DLCOfferTLV)

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

case class ContractInfoV0TLV(outcomes: Map[Sha256DigestBE, Satoshis])
    extends ContractInfoTLV {
  override val tpe: BigSizeUInt = ContractInfoV0TLV.tpe

  override val value: ByteVector = {
    outcomes.foldLeft(ByteVector.empty) {
      case (bytes, (outcome, amt)) =>
        bytes ++ outcome.bytes ++ UInt64(amt.toLong).bytes
    }
  }
}

object ContractInfoV0TLV extends TLVFactory[ContractInfoV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42768)

  override def fromTLVValue(value: ByteVector): ContractInfoV0TLV = {
    val iter = ValueIterator(value)

    val builder = Map.newBuilder[Sha256DigestBE, Satoshis]

    while (iter.index <= value.length - 40) {
      val outcome = Sha256DigestBE(iter.take(32))
      val amt = Satoshis(UInt64(iter.takeBits(64)).toLong)
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
      UInt64(totalCollateralSatoshis.toLong).bytes ++
      UInt16(fundingInputs.length).bytes ++
      fundingInputs.foldLeft(ByteVector.empty)(_ ++ _.bytes) ++
      changeSPK.bytes ++
      UInt64(feeRatePerKW.toLong).bytes ++
      contractMaturityBound.toUInt32.bytes ++
      contractTimeout.toUInt32.bytes
  }
}

object DLCOfferTLV extends TLVFactory[DLCOfferTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(42774)

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
    val totalCollateralSatoshis = Satoshis(UInt64(iter.takeBits(64)).toLong)
    val numFundingInputs = UInt16(iter.takeBits(16))
    val fundingInputs = (0 until numFundingInputs.toInt).toVector.map { _ =>
      val fundingInput = FundingInputTempTLV.fromBytes(iter.current)
      iter.skip(fundingInput)
      fundingInput
    }
    val changeSPK = ScriptPubKey(iter.current)
    iter.skip(changeSPK)
    val feeRatePerKW = SatoshisPerKW(Satoshis(UInt64(iter.takeBits(64)).toLong))
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
