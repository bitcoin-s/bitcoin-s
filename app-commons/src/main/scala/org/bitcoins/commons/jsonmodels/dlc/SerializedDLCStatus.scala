package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.tlv.{
  ContractInfoTLV,
  DLCOutcomeType,
  EnumOutcome,
  UnsignedNumericOutcome
}
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.crypto._
import scodec.bits.ByteVector
import ujson._

sealed trait SerializedDLCStatus {
  def paramHash: Sha256DigestBE
  def isInitiator: Boolean
  def state: DLCState
  def tempContractId: Sha256Digest
  def oracleInfo: OracleInfo
  def contractInfo: ContractInfo
  def timeouts: DLCTimeouts
  def feeRate: FeeUnit
  def totalCollateral: CurrencyUnit
  def localCollateral: CurrencyUnit
  def remoteCollateral: CurrencyUnit = totalCollateral - localCollateral

  lazy val statusString: String = state.toString

  def toJson: Value
}

sealed trait AcceptedSerializedDLCStatus extends SerializedDLCStatus {
  def contractId: ByteVector
}

sealed trait BroadcastedSerializedDLCStatus
    extends AcceptedSerializedDLCStatus {
  def fundingTxId: DoubleSha256DigestBE
}

sealed trait ClosedSerializedDLCStatus extends BroadcastedSerializedDLCStatus {
  def closingTxId: DoubleSha256DigestBE
}

sealed trait ClaimedSerializedDLCStatus extends ClosedSerializedDLCStatus {
  def outcome: DLCOutcomeType
  def oracleSigs: Vector[SchnorrDigitalSignature]
}

object SerializedDLCStatus {

  case class SerializedOffered(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit
  ) extends SerializedDLCStatus {
    override val state: DLCState = DLCState.Offered

    override lazy val toJson: Value = Obj(
      "state" -> Str(statusString),
      "paramHash" -> Str(paramHash.hex),
      "isInitiator" -> Bool(isInitiator),
      "tempContractId" -> Str(tempContractId.hex),
      "oracleInfo" -> Str(oracleInfo.hex),
      "contractInfo" -> Str(contractInfo.toTLV.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble)
    )
  }

  case class SerializedAccepted(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit)
      extends AcceptedSerializedDLCStatus {
    override val state: DLCState = DLCState.Accepted

    override lazy val toJson: Value = Obj(
      "state" -> Str(statusString),
      "paramHash" -> Str(paramHash.hex),
      "isInitiator" -> Bool(isInitiator),
      "tempContractId" -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "oracleInfo" -> Str(oracleInfo.hex),
      "contractInfo" -> Str(contractInfo.toTLV.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble)
    )
  }

  case class SerializedSigned(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit)
      extends AcceptedSerializedDLCStatus {
    override val state: DLCState = DLCState.Signed

    override lazy val toJson: Value = Obj(
      "state" -> Str(statusString),
      "paramHash" -> Str(paramHash.hex),
      "isInitiator" -> Bool(isInitiator),
      "tempContractId" -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "oracleInfo" -> Str(oracleInfo.hex),
      "contractInfo" -> Str(contractInfo.toTLV.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble)
    )
  }

  case class SerializedBroadcasted(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE)
      extends BroadcastedSerializedDLCStatus {
    override val state: DLCState = DLCState.Broadcasted

    override lazy val toJson: Value = Obj(
      "state" -> Str(statusString),
      "paramHash" -> Str(paramHash.hex),
      "isInitiator" -> Bool(isInitiator),
      "tempContractId" -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "oracleInfo" -> Str(oracleInfo.hex),
      "contractInfo" -> Str(contractInfo.toTLV.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
      "fundingTxId" -> Str(fundingTxId.hex)
    )
  }

  case class SerializedConfirmed(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE)
      extends BroadcastedSerializedDLCStatus {
    override val state: DLCState = DLCState.Confirmed

    override lazy val toJson: Value = Obj(
      "state" -> Str(statusString),
      "paramHash" -> Str(paramHash.hex),
      "isInitiator" -> Bool(isInitiator),
      "tempContractId" -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "oracleInfo" -> Str(oracleInfo.hex),
      "contractInfo" -> Str(contractInfo.toTLV.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
      "fundingTxId" -> Str(fundingTxId.hex)
    )
  }

  case class SerializedClaimed(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE,
      closingTxId: DoubleSha256DigestBE,
      oracleSigs: Vector[SchnorrDigitalSignature],
      outcome: DLCOutcomeType)
      extends ClaimedSerializedDLCStatus {
    override val state: DLCState = DLCState.Claimed

    override lazy val toJson: Value = {
      val outcomeJs = outcome match {
        case EnumOutcome(outcome) =>
          Str(outcome)
        case UnsignedNumericOutcome(digits) =>
          Arr.from(digits.map(num => Num(num)))
      }

      Obj(
        "state" -> Str(statusString),
        "paramHash" -> Str(paramHash.hex),
        "isInitiator" -> Bool(isInitiator),
        "tempContractId" -> Str(tempContractId.hex),
        "contractId" -> Str(contractId.toHex),
        "oracleInfo" -> Str(oracleInfo.hex),
        "contractInfo" -> Str(contractInfo.toTLV.hex),
        "contractMaturity" -> Num(
          timeouts.contractMaturity.toUInt32.toLong.toDouble),
        "contractTimeout" -> Num(
          timeouts.contractTimeout.toUInt32.toLong.toDouble),
        "feeRate" -> Num(feeRate.toLong.toDouble),
        "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
        "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
        "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
        "fundingTxId" -> Str(fundingTxId.hex),
        "closingTxId" -> Str(closingTxId.hex),
        "oracleSigs" -> oracleSigs.map(sig => Str(sig.hex)),
        "outcome" -> outcomeJs
      )
    }
  }

  case class SerializedRemoteClaimed(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE,
      closingTxId: DoubleSha256DigestBE,
      oracleSig: SchnorrDigitalSignature,
      outcome: DLCOutcomeType)
      extends ClaimedSerializedDLCStatus {
    override val state: DLCState = DLCState.RemoteClaimed
    override val oracleSigs: Vector[SchnorrDigitalSignature] = Vector(oracleSig)

    override lazy val toJson: Value = {
      val outcomeJs = outcome match {
        case EnumOutcome(outcome) =>
          Str(outcome)
        case UnsignedNumericOutcome(digits) =>
          Arr.from(digits.map(num => Num(num)))
      }

      Obj(
        "state" -> Str(statusString),
        "paramHash" -> Str(paramHash.hex),
        "isInitiator" -> Bool(isInitiator),
        "tempContractId" -> Str(tempContractId.hex),
        "contractId" -> Str(contractId.toHex),
        "oracleInfo" -> Str(oracleInfo.hex),
        "contractInfo" -> Str(contractInfo.toTLV.hex),
        "contractMaturity" -> Num(
          timeouts.contractMaturity.toUInt32.toLong.toDouble),
        "contractTimeout" -> Num(
          timeouts.contractTimeout.toUInt32.toLong.toDouble),
        "feeRate" -> Num(feeRate.toLong.toDouble),
        "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
        "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
        "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
        "fundingTxId" -> Str(fundingTxId.hex),
        "closingTxId" -> Str(closingTxId.hex),
        "oracleSigs" -> oracleSigs.map(sig => Str(sig.hex)),
        "outcome" -> outcomeJs
      )
    }
  }

  case class SerializedRefunded(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE,
      closingTxId: DoubleSha256DigestBE)
      extends ClosedSerializedDLCStatus {
    override val state: DLCState = DLCState.Refunded

    override lazy val toJson: Value =
      Obj(
        "state" -> Str(statusString),
        "paramHash" -> Str(paramHash.hex),
        "isInitiator" -> Bool(isInitiator),
        "tempContractId" -> Str(tempContractId.hex),
        "contractId" -> Str(contractId.toHex),
        "oracleInfo" -> Str(oracleInfo.hex),
        "contractInfo" -> Str(contractInfo.toTLV.hex),
        "contractMaturity" -> Num(
          timeouts.contractMaturity.toUInt32.toLong.toDouble),
        "contractTimeout" -> Num(
          timeouts.contractTimeout.toUInt32.toLong.toDouble),
        "feeRate" -> Num(feeRate.toLong.toDouble),
        "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
        "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
        "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
        "fundingTxId" -> Str(fundingTxId.hex),
        "closingTxId" -> Str(closingTxId.hex)
      )
  }

  def getContractId(status: SerializedDLCStatus): Option[ByteVector] = {
    status match {
      case status: AcceptedSerializedDLCStatus =>
        Some(status.contractId)
      case _: SerializedOffered | _: SerializedAccepted =>
        None
    }
  }

  def getFundingTxId(
      status: SerializedDLCStatus): Option[DoubleSha256DigestBE] = {
    status match {
      case status: BroadcastedSerializedDLCStatus =>
        Some(status.fundingTxId)
      case _: SerializedOffered | _: SerializedAccepted | _: SerializedSigned =>
        None
    }
  }

  def getClosingTxId(
      status: SerializedDLCStatus): Option[DoubleSha256DigestBE] = {
    status match {
      case status: ClosedSerializedDLCStatus =>
        Some(status.closingTxId)
      case _: SerializedOffered | _: AcceptedSerializedDLCStatus =>
        None
    }
  }

  def getOracleSignatures(
      status: SerializedDLCStatus): Option[Vector[SchnorrDigitalSignature]] = {
    status match {
      case claimed: ClaimedSerializedDLCStatus =>
        Some(claimed.oracleSigs)
      case _: SerializedOffered | _: SerializedAccepted | _: SerializedSigned |
          _: BroadcastedSerializedDLCStatus | _: SerializedRefunded =>
        None
    }
  }

  def fromJson(json: Value): SerializedDLCStatus = {
    val obj = json.obj

    val paramHash = Sha256DigestBE(obj("paramHash").str)
    val state = DLCState.fromString(obj("state").str)
    val isInitiator = obj("isInitiator").bool
    val tempContractId = Sha256Digest(obj("tempContractId").str)
    val oracleInfo = OracleInfo(obj("oracleInfo").str)
    val contractInfoTLV = ContractInfoTLV(obj("contractInfo").str)
    val contractMaturity = BlockStamp(
      UInt32(obj("contractMaturity").num.toLong))
    val contractTimeout = BlockStamp(UInt32(obj("contractTimeout").num.toLong))
    val feeRate = SatoshisPerVirtualByte.fromLong(obj("feeRate").num.toLong)
    val totalCollateral = Satoshis(obj("totalCollateral").num.toLong)
    val localCollateral = Satoshis(obj("localCollateral").num.toLong)

    lazy val contractId = ByteVector.fromValidHex(obj("contractId").str)
    lazy val fundingTxId = DoubleSha256DigestBE(obj("fundingTxId").str)
    lazy val closingTxId = DoubleSha256DigestBE(obj("closingTxId").str)
    lazy val oracleSigs =
      obj("oracleSigs").arr
        .map(value => SchnorrDigitalSignature(value.str))
        .toVector

    lazy val outcomeJs = obj("outcome")
    lazy val outcome = outcomeJs.strOpt match {
      case Some(value) => EnumOutcome(value)
      case None =>
        val digits = outcomeJs.arr.map(value => value.num.toInt)
        UnsignedNumericOutcome(digits.toVector)
    }

    state match {
      case DLCState.Offered =>
        SerializedOffered(
          paramHash,
          isInitiator,
          tempContractId,
          oracleInfo,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral
        )
      case DLCState.Accepted =>
        SerializedAccepted(
          paramHash,
          isInitiator,
          tempContractId,
          contractId,
          oracleInfo,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral
        )
      case DLCState.Signed =>
        SerializedSigned(
          paramHash,
          isInitiator,
          tempContractId,
          contractId,
          oracleInfo,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral
        )
      case DLCState.Broadcasted =>
        SerializedBroadcasted(
          paramHash,
          isInitiator,
          tempContractId,
          contractId,
          oracleInfo,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId
        )
      case DLCState.Confirmed =>
        SerializedConfirmed(
          paramHash,
          isInitiator,
          tempContractId,
          contractId,
          oracleInfo,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId
        )
      case DLCState.Claimed =>
        SerializedClaimed(
          paramHash,
          isInitiator,
          tempContractId,
          contractId,
          oracleInfo,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          closingTxId,
          oracleSigs,
          outcome
        )
      case DLCState.RemoteClaimed =>
        require(oracleSigs.size == 1,
                "Remote claimed should only have one oracle sig")
        SerializedRemoteClaimed(
          paramHash,
          isInitiator,
          tempContractId,
          contractId,
          oracleInfo,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          closingTxId,
          oracleSigs.head,
          outcome
        )
      case DLCState.Refunded =>
        SerializedRefunded(
          paramHash,
          isInitiator,
          tempContractId,
          contractId,
          oracleInfo,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          closingTxId
        )

    }
  }
}
