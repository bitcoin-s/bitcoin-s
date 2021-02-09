package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.dlc.DLCMessage._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.crypto._
import scodec.bits.ByteVector

sealed trait DLCStatus {

  /** The flipped sha256 hash of oracleInfo ++ contractInfo ++ timeoutes */
  def paramHash: Sha256DigestBE
  def isInitiator: Boolean
  def state: DLCState
  def tempContractId: Sha256Digest
  def contractInfo: ContractInfo
  def oracleInfo: OracleInfo = contractInfo.oracleInfo
  def timeouts: DLCTimeouts
  def feeRate: FeeUnit
  def totalCollateral: CurrencyUnit
  def localCollateral: CurrencyUnit
  def remoteCollateral: CurrencyUnit = totalCollateral - localCollateral

  lazy val statusString: String = state.toString
}

sealed trait AcceptedDLCStatus extends DLCStatus {
  def contractId: ByteVector
}

sealed trait BroadcastedDLCStatus extends AcceptedDLCStatus {
  def fundingTxId: DoubleSha256DigestBE
}

sealed trait ClosedDLCStatus extends BroadcastedDLCStatus {
  def closingTxId: DoubleSha256DigestBE
}

sealed trait ClaimedDLCStatus extends ClosedDLCStatus {
  def oracleOutcome: OracleOutcome
  def oracleSigs: Vector[SchnorrDigitalSignature]
}

object DLCStatus {

  case class Offered(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit
  ) extends DLCStatus {
    override val state: DLCState.Offered.type = DLCState.Offered
  }

  case class Accepted(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit)
      extends AcceptedDLCStatus {
    override val state: DLCState.Accepted.type = DLCState.Accepted
  }

  case class Signed(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit)
      extends AcceptedDLCStatus {
    override val state: DLCState.Signed.type = DLCState.Signed
  }

  case class Broadcasted(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE)
      extends BroadcastedDLCStatus {
    override val state: DLCState.Broadcasted.type = DLCState.Broadcasted
  }

  case class Confirmed(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE)
      extends BroadcastedDLCStatus {
    override val state: DLCState.Confirmed.type = DLCState.Confirmed
  }

  case class Claimed(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE,
      closingTxId: DoubleSha256DigestBE,
      oracleSigs: Vector[SchnorrDigitalSignature],
      oracleOutcome: OracleOutcome)
      extends ClaimedDLCStatus {
    override val state: DLCState.Claimed.type = DLCState.Claimed
  }

  case class RemoteClaimed(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE,
      closingTxId: DoubleSha256DigestBE,
      oracleSig: SchnorrDigitalSignature,
      oracleOutcome: OracleOutcome)
      extends ClaimedDLCStatus {
    override val state: DLCState.RemoteClaimed.type = DLCState.RemoteClaimed
    override val oracleSigs: Vector[SchnorrDigitalSignature] = Vector(oracleSig)
  }

  case class Refunded(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE,
      closingTxId: DoubleSha256DigestBE)
      extends ClosedDLCStatus {
    override val state: DLCState.Refunded.type = DLCState.Refunded
  }

  def getContractId(status: DLCStatus): Option[ByteVector] = {
    status match {
      case status: AcceptedDLCStatus =>
        Some(status.contractId)
      case _: Offered | _: Accepted =>
        None
    }
  }

  def getFundingTxId(status: DLCStatus): Option[DoubleSha256DigestBE] = {
    status match {
      case status: BroadcastedDLCStatus =>
        Some(status.fundingTxId)
      case _: Offered | _: Accepted | _: Signed =>
        None
    }
  }

  def getClosingTxId(status: DLCStatus): Option[DoubleSha256DigestBE] = {
    status match {
      case status: ClosedDLCStatus =>
        Some(status.closingTxId)
      case _: Offered | _: AcceptedDLCStatus =>
        None
    }
  }

  def getOracleSignatures(
      status: DLCStatus): Option[Vector[SchnorrDigitalSignature]] = {
    status match {
      case claimed: ClaimedDLCStatus =>
        Some(claimed.oracleSigs)
      case _: Offered | _: Accepted | _: Signed | _: BroadcastedDLCStatus |
          _: Refunded =>
        None
    }
  }

  def calculateOutcomeAndSig(
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      cet: Transaction): (SchnorrDigitalSignature, OracleOutcome) = {
    val localAdaptorSigs = if (isInitiator) {
      sign.cetSigs.outcomeSigs
    } else {
      accept.cetSigs.outcomeSigs
    }

    DLCUtil.computeOutcome(isInitiator,
                           offer.pubKeys.fundingKey,
                           accept.pubKeys.fundingKey,
                           offer.contractInfo,
                           localAdaptorSigs,
                           cet)
  }
}
