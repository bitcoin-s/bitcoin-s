package org.bitcoins.core.protocol.dlc.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.dlc.accounting.{DLCAccounting, RateOfReturnUtil}
import org.bitcoins.core.protocol.dlc.compute.DLCUtil
import org.bitcoins.core.protocol.dlc.models.DLCMessage.{
  DLCAccept,
  DLCOffer,
  DLCSign
}
import org.bitcoins.core.protocol.tlv.OracleAnnouncementTLV
import org.bitcoins.core.protocol.transaction.WitnessTransaction
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import java.time.Instant

sealed trait DLCStatus {

  /** The flipped sha256 hash of oracleInfo ++ contractInfo ++ timeoutes */
  def dlcId: Sha256Digest
  def isInitiator: Boolean
  def state: DLCState
  def lastUpdated: Instant
  def tempContractId: Sha256Digest
  def contractInfo: ContractInfo
  def oracleInfos: Vector[OracleInfo] = contractInfo.oracleInfos
  def timeouts: DLCTimeouts
  def feeRate: FeeUnit
  def totalCollateral: CurrencyUnit
  def localCollateral: CurrencyUnit
  def remoteCollateral: CurrencyUnit = totalCollateral - localCollateral

  lazy val announcements: Vector[OracleAnnouncementTLV] = {
    oracleInfos.flatMap(_.singleOracleInfos.map(_.announcement))
  }

  lazy val eventIds: Vector[String] = {
    announcements.map(_.eventTLV.eventId)
  }

  lazy val statusString: String = state.toString
}

sealed trait AcceptedDLCStatus extends DLCStatus {
  def contractId: ByteVector
}

sealed trait SignedDLCStatus extends AcceptedDLCStatus {
  def fundingTxId: DoubleSha256DigestBE
}

sealed trait ClosedDLCStatus extends SignedDLCStatus {
  def closingTxId: DoubleSha256DigestBE
  def myPayout: CurrencyUnit
  def counterPartyPayout: CurrencyUnit

  def accounting: DLCAccounting = {
    DLCAccounting(dlcId,
                  localCollateral,
                  remoteCollateral,
                  myPayout,
                  counterPartyPayout)
  }

  def pnl: CurrencyUnit = accounting.pnl
  def rateOfReturn: BigDecimal = accounting.rateOfReturn

  def rateOfReturnPrettyPrint: String =
    RateOfReturnUtil.prettyPrint(rateOfReturn)
}

sealed trait ClaimedDLCStatus extends ClosedDLCStatus {
  def oracleOutcome: OracleOutcome
  def oracleSigs: Vector[SchnorrDigitalSignature]
}

object DLCStatus {

  case class Offered(
      dlcId: Sha256Digest,
      isInitiator: Boolean,
      lastUpdated: Instant,
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
      dlcId: Sha256Digest,
      isInitiator: Boolean,
      lastUpdated: Instant,
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
      dlcId: Sha256Digest,
      isInitiator: Boolean,
      lastUpdated: Instant,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE)
      extends SignedDLCStatus {
    override val state: DLCState.Signed.type = DLCState.Signed
  }

  case class Broadcasted(
      dlcId: Sha256Digest,
      isInitiator: Boolean,
      lastUpdated: Instant,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE)
      extends SignedDLCStatus {
    override val state: DLCState.Broadcasted.type = DLCState.Broadcasted
  }

  case class Confirmed(
      dlcId: Sha256Digest,
      isInitiator: Boolean,
      lastUpdated: Instant,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE)
      extends SignedDLCStatus {
    override val state: DLCState.Confirmed.type = DLCState.Confirmed
  }

  case class Claimed(
      dlcId: Sha256Digest,
      isInitiator: Boolean,
      lastUpdated: Instant,
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
      oracleOutcome: OracleOutcome,
      myPayout: CurrencyUnit,
      counterPartyPayout: CurrencyUnit)
      extends ClaimedDLCStatus {
    override val state: DLCState.Claimed.type = DLCState.Claimed
  }

  case class RemoteClaimed(
      dlcId: Sha256Digest,
      isInitiator: Boolean,
      lastUpdated: Instant,
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
      oracleOutcome: OracleOutcome,
      myPayout: CurrencyUnit,
      counterPartyPayout: CurrencyUnit)
      extends ClaimedDLCStatus {
    override val state: DLCState.RemoteClaimed.type = DLCState.RemoteClaimed
    override val oracleSigs: Vector[SchnorrDigitalSignature] = Vector(oracleSig)
  }

  case class Refunded(
      dlcId: Sha256Digest,
      isInitiator: Boolean,
      lastUpdated: Instant,
      tempContractId: Sha256Digest,
      contractId: ByteVector,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      totalCollateral: CurrencyUnit,
      localCollateral: CurrencyUnit,
      fundingTxId: DoubleSha256DigestBE,
      closingTxId: DoubleSha256DigestBE,
      myPayout: CurrencyUnit,
      counterPartyPayout: CurrencyUnit)
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
      case status: SignedDLCStatus =>
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
      case _: Offered | _: Accepted | _: SignedDLCStatus | _: Refunded =>
        None
    }
  }

  def calculateOutcomeAndSig(
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      cet: WitnessTransaction): Option[
    (SchnorrDigitalSignature, OracleOutcome)] = {
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
