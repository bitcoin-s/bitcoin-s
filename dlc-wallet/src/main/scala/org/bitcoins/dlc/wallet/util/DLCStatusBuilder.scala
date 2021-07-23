package org.bitcoins.dlc.wallet.util

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.dlc.accounting.DLCAccounting
import org.bitcoins.core.protocol.dlc.models.DLCStatus._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.SchnorrDigitalSignature
import org.bitcoins.dlc.wallet.accounting.AccountingUtil
import org.bitcoins.dlc.wallet.models._
import org.bitcoins.dlc.wallet.accounting.DLCAccountingDbs

object DLCStatusBuilder {

  /** Helper method to convert a bunch of indepdendent datastructures into a in progress dlc status */
  def buildInProgressDLCStatus(
      dlcDb: DLCDb,
      contractInfo: ContractInfo,
      contractData: DLCContractDataDb,
      offerDb: DLCOfferDb): DLCStatus = {
    require(
      dlcDb.state.isInstanceOf[DLCState.InProgressState],
      s"Cannot have divergent states beteween dlcDb and the parameter state, got= dlcDb.state=${dlcDb.state} state=${dlcDb.state}"
    )
    val dlcId = dlcDb.dlcId

    val totalCollateral = contractData.totalCollateral

    val localCollateral = if (dlcDb.isInitiator) {
      offerDb.collateral
    } else {
      totalCollateral - offerDb.collateral
    }

    val status = dlcDb.state.asInstanceOf[DLCState.InProgressState] match {
      case DLCState.Offered =>
        Offered(
          dlcId,
          dlcDb.isInitiator,
          dlcDb.tempContractId,
          contractInfo,
          contractData.dlcTimeouts,
          dlcDb.feeRate,
          totalCollateral,
          localCollateral
        )
      case DLCState.Accepted =>
        Accepted(
          dlcId,
          dlcDb.isInitiator,
          dlcDb.tempContractId,
          dlcDb.contractIdOpt.get,
          contractInfo,
          contractData.dlcTimeouts,
          dlcDb.feeRate,
          totalCollateral,
          localCollateral
        )
      case DLCState.Signed =>
        Signed(
          dlcId,
          dlcDb.isInitiator,
          dlcDb.tempContractId,
          dlcDb.contractIdOpt.get,
          contractInfo,
          contractData.dlcTimeouts,
          dlcDb.feeRate,
          totalCollateral,
          localCollateral
        )
      case DLCState.Broadcasted =>
        Broadcasted(
          dlcId,
          dlcDb.isInitiator,
          dlcDb.tempContractId,
          dlcDb.contractIdOpt.get,
          contractInfo,
          contractData.dlcTimeouts,
          dlcDb.feeRate,
          totalCollateral,
          localCollateral,
          dlcDb.fundingTxIdOpt.get
        )
      case DLCState.Confirmed =>
        Confirmed(
          dlcId,
          dlcDb.isInitiator,
          dlcDb.tempContractId,
          dlcDb.contractIdOpt.get,
          contractInfo,
          contractData.dlcTimeouts,
          dlcDb.feeRate,
          totalCollateral,
          localCollateral,
          dlcDb.fundingTxIdOpt.get
        )
    }

    status
  }

  def buildClosedDLCStatus(
      dlcDb: DLCDb,
      contractInfo: ContractInfo,
      contractData: DLCContractDataDb,
      nonceDbs: Vector[OracleNonceDb],
      announcementsWithId: Vector[(OracleAnnouncementV0TLV, Long)],
      announcementIds: Vector[DLCAnnouncementDb],
      offerDb: DLCOfferDb,
      acceptDb: DLCAcceptDb,
      closingTx: Transaction): ClosedDLCStatus = {
    require(
      dlcDb.state.isInstanceOf[DLCState.ClosedState],
      s"Cannot have divergent states beteween dlcDb and the parameter state, got= dlcDb.state=${dlcDb.state} state=${dlcDb.state}"
    )

    val dlcId = dlcDb.dlcId
    val financials = DLCAccountingDbs(dlcDb, offerDb, acceptDb, closingTx)
    val accounting: DLCAccounting =
      AccountingUtil.calculatePnl(financials)

    val totalCollateral = contractData.totalCollateral

    val localCollateral = if (dlcDb.isInitiator) {
      offerDb.collateral
    } else {
      totalCollateral - offerDb.collateral
    }
    val status = dlcDb.state.asInstanceOf[DLCState.ClosedState] match {
      case DLCState.Refunded =>
        //no oracle information in the refund case
        val refund = Refunded(
          dlcId,
          dlcDb.isInitiator,
          dlcDb.tempContractId,
          dlcDb.contractIdOpt.get,
          contractInfo,
          contractData.dlcTimeouts,
          dlcDb.feeRate,
          totalCollateral,
          localCollateral,
          dlcDb.fundingTxIdOpt.get,
          closingTx.txIdBE,
          myPayout = accounting.myPayout,
          counterPartyPayout = accounting.theirPayout
        )
        refund
      case oracleOutcomeState: DLCState.ClosedViaOracleOutcomeState =>
        val (oracleOutcome, sigs) = getOracleOutcomeAndSigs(
          announcementIds = announcementIds,
          announcementsWithId = announcementsWithId,
          nonceDbs = nonceDbs)

        oracleOutcomeState match {
          case DLCState.Claimed =>
            Claimed(
              dlcId,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              contractInfo,
              contractData.dlcTimeouts,
              dlcDb.feeRate,
              totalCollateral,
              localCollateral,
              dlcDb.fundingTxIdOpt.get,
              closingTx.txIdBE,
              sigs,
              oracleOutcome,
              myPayout = accounting.myPayout,
              counterPartyPayout = accounting.theirPayout
            )
          case DLCState.RemoteClaimed =>
            RemoteClaimed(
              dlcId,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              contractInfo,
              contractData.dlcTimeouts,
              dlcDb.feeRate,
              totalCollateral,
              localCollateral,
              dlcDb.fundingTxIdOpt.get,
              closingTx.txIdBE,
              dlcDb.aggregateSignatureOpt.get,
              oracleOutcome,
              myPayout = accounting.myPayout,
              counterPartyPayout = accounting.theirPayout
            )
        }
    }
    status
  }

  /** Calculates oracle outcome and signatures. Returns none if the dlc is not in a valid state to
    * calculate the outcome
    */
  def getOracleOutcomeAndSigs(
      announcementIds: Vector[DLCAnnouncementDb],
      announcementsWithId: Vector[(OracleAnnouncementV0TLV, Long)],
      nonceDbs: Vector[OracleNonceDb]): (
      OracleOutcome,
      Vector[SchnorrDigitalSignature]) = {
    val noncesByAnnouncement =
      nonceDbs.sortBy(_.index).groupBy(_.announcementId)
    val oracleOutcome = {
      val usedOracleIds = announcementIds.filter(_.used)
      val usedOracles = usedOracleIds.sortBy(_.index).map { used =>
        announcementsWithId.find(_._2 == used.announcementId).get
      }
      require(usedOracles.nonEmpty, "Error, no oracles used")
      announcementsWithId.head._1.eventTLV.eventDescriptor match {
        case _: EnumEventDescriptorV0TLV =>
          val oracleInfos = usedOracles.map(t => EnumSingleOracleInfo(t._1))
          val outcomes = usedOracles.map { case (_, id) =>
            val nonces = noncesByAnnouncement(id)
            require(nonces.size == 1,
                    s"Only 1 outcome for enum, got ${nonces.size}")
            EnumOutcome(nonces.head.outcomeOpt.get)
          }
          require(outcomes.distinct.size == 1,
                  s"Should only be one outcome for enum, got $outcomes")
          EnumOracleOutcome(oracleInfos, outcomes.head)
        case _: UnsignedDigitDecompositionEventDescriptor =>
          val oraclesAndOutcomes = usedOracles.map { case (announcement, id) =>
            val oracleInfo = NumericSingleOracleInfo(announcement)
            val nonces = noncesByAnnouncement(id).sortBy(_.index)
            // need to allow for some Nones because we don't always get
            // all the digits because of prefixing
            val digits = nonces.flatMap(_.outcomeOpt.map(_.toInt))
            require(digits.nonEmpty, s"Got no digits for announcement id $id")
            val outcome = UnsignedNumericOutcome(digits)
            (oracleInfo, outcome)
          }
          NumericOracleOutcome(oraclesAndOutcomes)
        case _: SignedDigitDecompositionEventDescriptor =>
          throw new RuntimeException(s"SignedNumericOutcome not yet supported")
      }
    }

    val sigs = nonceDbs.flatMap(_.signatureOpt)
    (oracleOutcome, sigs)
  }
}
