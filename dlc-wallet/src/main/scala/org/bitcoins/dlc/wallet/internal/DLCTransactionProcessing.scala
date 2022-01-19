package org.bitcoins.dlc.wallet.internal

import org.bitcoins.core.api.dlc.wallet.db._
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.protocol.dlc.execution.SetupDLC
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.utxo.AddressTag
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.SafeDatabase
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.wallet.internal.TransactionProcessing

import scala.concurrent._

/** Overrides TransactionProcessing from Wallet to add extra logic to
  * process transactions that could from our own DLC.
  */
private[bitcoins] trait DLCTransactionProcessing extends TransactionProcessing {
  self: DLCWallet =>

  import dlcDAO.profile.api._
  private lazy val safeDatabase: SafeDatabase = dlcDAO.safeDatabase

  private lazy val dlcDataManagement: DLCDataManagement = DLCDataManagement(
    dlcWalletDAOs)

  /** Calculates the new state of the DLCDb based on the closing transaction,
    * will delete old CET sigs that are no longer needed after execution
    */
  def calculateAndSetState(dlcDb: DLCDb): Future[DLCDb] = {
    (dlcDb.contractIdOpt, dlcDb.closingTxIdOpt) match {
      case (Some(id), Some(txId)) =>
        val fundingInputsF = dlcInputsDAO.findByDLCId(dlcDb.dlcId)
        val scriptSigParamsF =
          fundingInputsF.flatMap(inputs => getScriptSigParams(dlcDb, inputs))

        for {
          scriptSigParams <- scriptSigParamsF
          executorWithSetupOpt <- dlcDataManagement.executorAndSetupFromDb(
            id,
            transactionDAO,
            scriptSigParams,
            keyManager)
          updatedDlcDb <- executorWithSetupOpt match {
            case Some(exeutorWithSetup) =>
              calculateAndSetStateWithSetupDLC(exeutorWithSetup.setup,
                                               dlcDb,
                                               txId)
            case None =>
              //this means we have already deleted the cet sigs
              //just return the dlcdb given to us
              Future.successful(dlcDb)
          }
        } yield updatedDlcDb
      case (None, None) | (None, Some(_)) | (Some(_), None) =>
        Future.successful(dlcDb)
    }
  }

  /** Calculates the new closing state for a DLC if we still
    * have adaptor signatures available to us in the database
    */
  private def calculateAndSetStateWithSetupDLC(
      setup: SetupDLC,
      dlcDb: DLCDb,
      closingTxId: DoubleSha256DigestBE): Future[DLCDb] = {
    val updatedF = if (closingTxId == setup.refundTx.txIdBE) {
      Future.successful(dlcDb.copy(state = DLCState.Refunded))
    } else if (dlcDb.state == DLCState.Claimed) {
      Future.successful(dlcDb.copy(state = DLCState.Claimed))
    } else {
      val withState = dlcDb.updateState(DLCState.RemoteClaimed)
      if (dlcDb.state != DLCState.RemoteClaimed) {
        for {
          // update so we can calculate correct DLCStatus
          _ <- dlcDAO.update(withState)
          withOutcome <- calculateAndSetOutcome(withState)
          dlc <- findDLC(dlcDb.dlcId)
          _ = dlcConfig.walletCallbacks.executeOnDLCStateChange(logger, dlc.get)
        } yield withOutcome
      } else Future.successful(withState)
    }

    for {
      updated <- updatedF
      _ <- {
        updated.state match {
          case DLCState.Claimed | DLCState.RemoteClaimed | DLCState.Refunded =>
            val contractId = updated.contractIdOpt.get.toHex
            logger.info(
              s"Deleting unneeded DLC signatures for contract $contractId")

            dlcSigsDAO.deleteByDLCId(updated.dlcId)
          case DLCState.Offered | DLCState.Accepted | DLCState.Signed |
              DLCState.Broadcasted | DLCState.Confirmed =>
            FutureUtil.unit
        }
      }
    } yield updated
  }

  /** Calculates the outcome used for execution
    *  based on the closing transaction
    */
  def calculateAndSetOutcome(dlcDb: DLCDb): Future[DLCDb] = {
    if (dlcDb.state == DLCState.RemoteClaimed) {
      val dlcId = dlcDb.dlcId

      for {
        (_, contractData, offerDb, acceptDbOpt, fundingInputDbs, _) <-
          dlcDataManagement.getDLCFundingData(dlcId)
        txIds = fundingInputDbs.map(_.outPoint.txIdBE)
        remotePrevTxs <- remoteTxDAO.findByTxIdBEs(txIds)
        localPrevTxs <- transactionDAO.findByTxIdBEs(txIds)
        (sigDbs, refundSigsDb) <- dlcDataManagement.getCetAndRefundSigs(dlcId)
        (announcements, announcementData, nonceDbs) <- dlcDataManagement
          .getDLCAnnouncementDbs(dlcDb.dlcId)

        cet <-
          transactionDAO
            .read(dlcDb.closingTxIdOpt.get)
            .map(_.get.transaction.asInstanceOf[WitnessTransaction])

        (sig, outcome) = {
          val prevTxs = (remotePrevTxs ++ localPrevTxs).map(_.transaction)
          val txs = prevTxs.groupBy(_.txIdBE)

          val isInit = dlcDb.isInitiator

          val fundingInputs = fundingInputDbs.map(input =>
            input.toFundingInput(txs(input.outPoint.txIdBE).head))

          val offerRefundSigOpt = refundSigsDb.flatMap(_.initiatorSig)
          val acceptRefundSigOpt = refundSigsDb.map(_.accepterSig)

          val contractInfo =
            dlcDataManagement.getContractInfo(contractData,
                                              announcements,
                                              announcementData,
                                              nonceDbs)

          val offer = {
            offerDb.toDLCOffer(contractInfo, fundingInputs, dlcDb, contractData)
          }
          require(
            acceptDbOpt.isDefined,
            s"Cannot calculate and set outcome if we haven't seen an accept message, dlcId=${dlcId.hex}")
          val acceptDb = acceptDbOpt.get
          val accept =
            acceptDb.toDLCAccept(
              dlcDb.tempContractId,
              fundingInputs,
              sigDbs.map(dbSig => (dbSig.sigPoint, dbSig.accepterSig)),
              acceptRefundSigOpt.head)

          val sign: DLCSign = {
            val cetSigs: CETSignatures =
              CETSignatures(
                sigDbs.map(dbSig => (dbSig.sigPoint, dbSig.initiatorSig.get)))

            val contractId = dlcDb.contractIdOpt.get
            val fundingSigs =
              fundingInputDbs
                .filter(_.isInitiator)
                .map { input =>
                  input.witnessScriptOpt match {
                    case Some(witnessScript) =>
                      witnessScript match {
                        case EmptyScriptWitness =>
                          throw new RuntimeException(
                            "Script witness cannot be empty")
                        case witness: ScriptWitnessV0 =>
                          (input.outPoint, witness)
                      }
                    case None =>
                      throw new RuntimeException("Must be segwit")
                  }
                }

            DLCSign(cetSigs,
                    offerRefundSigOpt.get,
                    FundingSignatures(fundingSigs),
                    contractId)
          }

          DLCStatus.calculateOutcomeAndSig(isInit, offer, accept, sign, cet).get
        }
        (_, oracleInfos) = getOutcomeDbInfo(outcome)

        noncesByAnnouncement = nonceDbs
          .groupBy(_.announcementId)

        announcementsWithId = dlcDataManagement.getOracleAnnouncementsWithId(
          announcements,
          announcementData,
          nonceDbs)

        usedIds = announcementsWithId
          .filter(t => oracleInfos.exists(_.announcement == t._1))
          .map(_._2)

        updatedAnnouncements = announcements
          .filter(t => usedIds.contains(t.announcementId))
          .map(_.copy(used = true))

        updatedNonces = {
          usedIds.flatMap { id =>
            outcome match {
              case enum: EnumOracleOutcome =>
                val nonces = noncesByAnnouncement(id).sortBy(_.index)
                nonces.map(_.copy(outcomeOpt = Some(enum.outcome.outcome)))
              case numeric: NumericOracleOutcome =>
                numeric.oraclesAndOutcomes.flatMap { case (oracle, outcome) =>
                  val id = announcementsWithId
                    .find(_._1 == oracle.announcement)
                    .map(_._2)
                    .get
                  val nonces = noncesByAnnouncement(id).sortBy(_.index)
                  outcome.digits.zip(nonces).map { case (digit, nonceDb) =>
                    nonceDb.copy(outcomeOpt = Some(digit.toString))
                  }
                }
            }

          }
        }
        updatedDlcDbA = dlcDAO.updateAction(
          dlcDb.copy(aggregateSignatureOpt = Some(sig)))
        updateNonceA = oracleNonceDAO.updateAllAction(updatedNonces)
        updateAnnouncementA = dlcAnnouncementDAO.updateAllAction(
          updatedAnnouncements)
        actions = DBIO
          .seq(updatedDlcDbA, updateNonceA, updateAnnouncementA)
          .transactionally
        _ <- safeDatabase.run(actions)
      } yield {
        dlcDb.copy(aggregateSignatureOpt = Some(sig))
      }
    } else {
      Future.successful(dlcDb)
    }
  }

  /** Process incoming utxos as normal, and then update the DLC states if applicable */
  override protected def processReceivedUtxos(
      tx: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      spendingInfoDbs: Vector[SpendingInfoDb],
      newTags: Vector[AddressTag]): Future[Vector[SpendingInfoDb]] = {
    super
      .processReceivedUtxos(tx, blockHashOpt, spendingInfoDbs, newTags)
      .flatMap { res =>
        for {
          dlcDbs <- dlcDAO.findByFundingTxId(tx.txIdBE)
          _ <-
            if (dlcDbs.nonEmpty) {
              logger.info(
                s"Processing tx ${tx.txIdBE.hex} for ${dlcDbs.size} DLC(s)")
              insertTransaction(tx, blockHashOpt)
            } else FutureUtil.unit

          // Update the state to be confirmed or broadcasted
          updated = dlcDbs.map { dlcDb =>
            dlcDb.state match {
              case DLCState.Offered | DLCState.Accepted | DLCState.Signed |
                  DLCState.Broadcasted =>
                if (blockHashOpt.isDefined)
                  dlcDb.updateState(DLCState.Confirmed)
                else dlcDb.copy(state = DLCState.Broadcasted)
              case DLCState.Confirmed | DLCState.Claimed |
                  DLCState.RemoteClaimed | DLCState.Refunded =>
                dlcDb
            }
          }

          _ <- dlcDAO.updateAll(updated)
          dlcIds = updated.map(_.dlcId).distinct
          updatedDlcDbs <- Future.sequence(dlcIds.map(findDLC))
          _ <- Future.sequence {
            updatedDlcDbs.map(u =>
              dlcConfig.walletCallbacks.executeOnDLCStateChange(logger, u.get))
          }
        } yield res
      }
  }

  override protected def processSpentUtxos(
      transaction: Transaction,
      outputsBeingSpent: Vector[SpendingInfoDb],
      blockHashOpt: Option[DoubleSha256DigestBE]): Future[
    Vector[SpendingInfoDb]] = {
    super
      .processSpentUtxos(transaction, outputsBeingSpent, blockHashOpt)
      .flatMap { res =>
        val outPoints = transaction.inputs.map(_.previousOutput).toVector

        for {
          dlcDbs <- dlcDAO.findByFundingOutPoints(outPoints)
          _ <-
            if (dlcDbs.nonEmpty) {
              logger.info(
                s"Processing tx ${transaction.txIdBE.hex} for ${dlcDbs.size} DLC(s)")
              insertTransaction(transaction, blockHashOpt)
            } else FutureUtil.unit

          withTx = dlcDbs.map(_.updateClosingTxId(transaction.txIdBE))
          updatedFs = withTx.map(calculateAndSetState)
          updated <- Future.sequence(updatedFs)
          _ <- dlcDAO.updateAll(updated)
        } yield res
      }
  }

  private def getOutcomeDbInfo(oracleOutcome: OracleOutcome): (
      Vector[DLCOutcomeType],
      Vector[SingleOracleInfo]) = {
    oracleOutcome match {
      case EnumOracleOutcome(oracles, outcome) =>
        (Vector.fill(oracles.length)(outcome), oracles)
      case numeric: NumericOracleOutcome =>
        (numeric.outcomes, numeric.oracles)
    }
  }
}
