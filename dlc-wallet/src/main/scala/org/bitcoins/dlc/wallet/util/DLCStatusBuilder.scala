package org.bitcoins.dlc.wallet.util

import org.bitcoins.core.dlc.accounting.DLCAccounting
import org.bitcoins.core.protocol.dlc.models.{
  ClosedDLCStatus,
  ContractInfo,
  DLCState,
  DLCStatus,
  OracleOutcome
}
import org.bitcoins.core.protocol.dlc.models.DLCStatus.{
  Accepted,
  Broadcasted,
  Claimed,
  Confirmed,
  Offered,
  Refunded,
  RemoteClaimed,
  Signed
}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.SchnorrDigitalSignature
import org.bitcoins.dlc.wallet.accounting.AccountingUtil
import org.bitcoins.dlc.wallet.models.{
  DLCAcceptDb,
  DLCContractDataDb,
  DLCDb,
  DLCOfferDb,
  OracleNonceDb
}

import scala.concurrent.{ExecutionContext, Future}

object DLCStatusBuilder {

  /** Helper method to convert a bunch of indepdendent datastructures into a in progress dlc status */
  def buildInProgressDLCStatus(
      state: DLCState.InProgressState,
      dlcDb: DLCDb,
      contractInfo: ContractInfo,
      contractData: DLCContractDataDb,
      offerDb: DLCOfferDb): DLCStatus = {
    require(
      dlcDb.state == state,
      s"Cannot have divergent states beteween dlcDb and the parameter state, got= dlcDb.state=${dlcDb.state} state=${state}")
    val dlcId = dlcDb.dlcId

    val totalCollateral = contractData.totalCollateral

    val localCollateral = if (dlcDb.isInitiator) {
      offerDb.collateral
    } else {
      totalCollateral - offerDb.collateral
    }

    val status = state match {
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
      state: DLCState.ClosedState,
      dlcDb: DLCDb,
      contractInfo: ContractInfo,
      contractData: DLCContractDataDb,
      nonceDbs: Vector[OracleNonceDb],
      offerDb: DLCOfferDb,
      acceptDb: DLCAcceptDb,
      closingTx: Transaction)(implicit
      ec: ExecutionContext): Future[ClosedDLCStatus] = {
    require(
      dlcDb.state == state,
      s"Cannot have divergent states beteween dlcDb and the parameter state, got= dlcDb.state=${dlcDb.state} state=${state}")
    val dlcId = dlcDb.dlcId
    val accounting: DLCAccounting =
      AccountingUtil.calculatePnl(dlcDb, offerDb, acceptDb, closingTx)

    //start calculation up here in parallel as this is a bottleneck currently
    val outcomesOptF: Future[
      Option[(OracleOutcome, Vector[SchnorrDigitalSignature])]] =
      for {
        oracleOutcomeSigsOpt <- getOracleOutcomeAndSigs(dlcDb = dlcDb,
                                                        contractInfo =
                                                          contractInfo,
                                                        nonceDbs = nonceDbs)
      } yield oracleOutcomeSigsOpt

    val totalCollateral = contractData.totalCollateral

    val localCollateral = if (dlcDb.isInitiator) {
      offerDb.collateral
    } else {
      totalCollateral - offerDb.collateral
    }
    val statusF = state match {
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
        Future.successful(refund)
      case oracleOutcomeState: DLCState.ClosedViaOracleOutcomeState =>
        //a state that requires an oracle outcome
        //the .get below should always be valid
        for {
          outcomesOpt <- outcomesOptF
          (oracleOutcome, sigs) = outcomesOpt.get
        } yield {
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
    }
    statusF
  }

  /** Calculates oracle outcome and signatures. Returns none if the dlc is not in a valid state to
    * calculate the outcome
    */
  def getOracleOutcomeAndSigs(
      dlcDb: DLCDb,
      contractInfo: ContractInfo,
      nonceDbs: Vector[OracleNonceDb])(implicit ec: ExecutionContext): Future[
    Option[(OracleOutcome, Vector[SchnorrDigitalSignature])]] = {
    Future {
      dlcDb.aggregateSignatureOpt match {
        case Some(aggSig) =>
          val outcome =
            contractInfo.sigPointMap(aggSig.sig.toPrivateKey.publicKey)
          val sigs = nonceDbs.flatMap(_.signatureOpt)
          Some((outcome, sigs))
        case None => None
      }
    }
  }
}
