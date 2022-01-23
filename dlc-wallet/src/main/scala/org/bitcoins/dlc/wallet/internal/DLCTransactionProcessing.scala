package org.bitcoins.dlc.wallet.internal

import org.bitcoins.core.api.dlc.wallet.db._
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.protocol.dlc.execution.SetupDLC
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.utxo.AddressTag
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.SafeDatabase
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.dlc.wallet.models.{
  AcceptDbState,
  DLCCETSignaturesDb,
  DLCFundingInputDb,
  OfferedDbState
}
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
    * @return a DLCDb if we can calculate the state, else None if we cannot calculate the state
    */
  def calculateAndSetState(dlcDb: DLCDb): Future[Option[DLCDb]] = {
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
              Future.successful(Some(dlcDb))
          }
        } yield {
          updatedDlcDb
        }
      case (None, None) | (None, Some(_)) | (Some(_), None) =>
        Future.successful(Some(dlcDb))
    }
  }

  /** Calculates the new closing state for a DLC if we still
    * have adaptor signatures available to us in the database
    */
  private def calculateAndSetStateWithSetupDLC(
      setup: SetupDLC,
      dlcDb: DLCDb,
      closingTxId: DoubleSha256DigestBE): Future[Option[DLCDb]] = {
    val updatedOptF: Future[Option[DLCDb]] = {
      if (closingTxId == setup.refundTx.txIdBE) {
        val updatedOpt = Some(dlcDb.copy(state = DLCState.Refunded))
        Future.successful(updatedOpt)
      } else if (dlcDb.state == DLCState.Claimed) {
        val updatedOpt = Some(dlcDb.copy(state = DLCState.Claimed))
        Future.successful(updatedOpt)
      } else {
        val withState = dlcDb.updateState(DLCState.RemoteClaimed)
        if (dlcDb.state != DLCState.RemoteClaimed) {
          for {
            // update so we can calculate correct DLCStatus
            _ <- dlcDAO.update(withState)
            withOutcomeOpt <- calculateAndSetOutcome(withState)
            dlc <- findDLC(dlcDb.dlcId)
            _ = dlcConfig.walletCallbacks.executeOnDLCStateChange(logger,
                                                                  dlc.get)
          } yield withOutcomeOpt
        } else Future.successful(Some(withState))
      }
    }

    for {
      updatedOpt <- updatedOptF
      _ <- {
        val resultFOpt: Option[Future[Unit]] = {
          updatedOpt.map { updated =>
            updated.state match {
              case DLCState.Claimed | DLCState.RemoteClaimed |
                  DLCState.Refunded =>
                val contractId = updated.contractIdOpt.get.toHex
                logger.info(
                  s"Deleting unneeded DLC signatures for contract $contractId")

                dlcSigsDAO
                  .deleteByDLCId(updated.dlcId)
                  .map(_ => ())
              case DLCState.Offered | DLCState.Accepted | DLCState.Signed |
                  DLCState.Broadcasted | DLCState.Confirmed =>
                FutureUtil.unit
            }
          }
        }
        resultFOpt match {
          case Some(resultF) => resultF
          case None          => Future.unit
        }
      }
    } yield updatedOpt
  }

  /** Calculates the outcome used for execution
    *  based on the closing transaction. Returns None if we cannot calculate
    *  the outcome because we have pruned the cet signatures from the database
    */
  def calculateAndSetOutcome(dlcDb: DLCDb): Future[Option[DLCDb]] = {
    if (dlcDb.state == DLCState.RemoteClaimed) {
      val dlcId = dlcDb.dlcId

      for {
        setupStateOpt <- dlcDataManagement.getDLCFundingData(dlcId,
                                                             txDAO =
                                                               transactionDAO)
        acceptDbState = {
          setupStateOpt.get match {
            case accept: AcceptDbState => accept
            case offered: OfferedDbState =>
              sys.error(
                s"Cannot calculate and set outcome of dlc that is only offered, id=${offered.dlcDb.dlcId.hex}")
          }
        }
        offer = acceptDbState.offer
        acceptOpt = acceptDbState.acceptOpt
        (sigDbs, refundSigsDb) <- dlcDataManagement.getCetAndRefundSigs(dlcId)
        (announcements, announcementData, nonceDbs) <- dlcDataManagement
          .getDLCAnnouncementDbs(dlcDb.dlcId)

        cet <-
          transactionDAO
            .read(dlcDb.closingTxIdOpt.get)
            .map(_.get.transaction.asInstanceOf[WitnessTransaction])

        sigAndOutcomeOpt = {
          val isInit = dlcDb.isInitiator

          val fundingInputDbs = acceptDbState.allFundingInputs
          val offerRefundSigOpt = refundSigsDb.flatMap(_.initiatorSig)

          val signOpt: Option[DLCSign] = offerRefundSigOpt.map { refundSig =>
            buildSignMessage(
              dlcDb = dlcDb,
              sigDbs = sigDbs,
              offerRefundSig = refundSig,
              fundingInputDbs = fundingInputDbs
            )
          }
          for {
            accept <- acceptOpt
            sign <- signOpt
            sigsAndOutcomeOpt <- DLCStatus.calculateOutcomeAndSig(isInit,
                                                                  offer,
                                                                  accept,
                                                                  sign,
                                                                  cet)
          } yield {
            sigsAndOutcomeOpt
          }
        }
        sigOpt = sigAndOutcomeOpt.map(_._1)
        outcomeOpt = sigAndOutcomeOpt.map(_._2)
        oracleInfosOpt = {
          outcomeOpt
            .map { case outcome =>
              getOutcomeDbInfo(outcome)
            }
            .map(_._2)
        }

        noncesByAnnouncement = nonceDbs
          .groupBy(_.announcementId)

        announcementsWithId = dlcDataManagement.getOracleAnnouncementsWithId(
          announcements,
          announcementData,
          nonceDbs)

        usedIds = {
          oracleInfosOpt match {
            case Some(oracleInfos) =>
              announcementsWithId
                .filter(t => oracleInfos.exists(_.announcement == t._1))
                .map(_._2)
            case None =>
              Vector.empty
          }
        }

        updatedAnnouncements = announcements
          .filter(t => usedIds.contains(t.announcementId))
          .map(_.copy(used = true))
        updatedNonces = {
          usedIds.flatMap { id =>
            outcomeOpt.map {
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
          }.flatten
        }
        updatedDlcDbA = dlcDAO.updateAction(
          dlcDb.copy(aggregateSignatureOpt = sigOpt))
        updateNonceA = oracleNonceDAO.updateAllAction(updatedNonces)
        updateAnnouncementA = dlcAnnouncementDAO.updateAllAction(
          updatedAnnouncements)
        actions = DBIO
          .seq(updatedDlcDbA, updateNonceA, updateAnnouncementA)
          .transactionally
        _ <- safeDatabase.run(actions)
      } yield {
        //err, this probably needs to be contingent on if sigOpt is defined?
        val updatedSig = dlcDb.copy(aggregateSignatureOpt = sigOpt)
        Some(updatedSig)
      }
    } else {
      Future.successful(Some(dlcDb))
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
                s"Processing received utxos in tx ${tx.txIdBE.hex} for ${dlcDbs.size} DLC(s)")
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
        } yield {
          res
        }
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
                s"Processing spent utxos in tx ${transaction.txIdBE.hex} for ${dlcDbs.size} DLC(s)")
              insertTransaction(transaction, blockHashOpt)
            } else FutureUtil.unit

          withTx = dlcDbs.map(_.updateClosingTxId(transaction.txIdBE))
          updatedFs = withTx.map(calculateAndSetState)
          updated <- Future.sequence(updatedFs)
          _ <- dlcDAO.updateAll(updated.flatten)
        } yield {
          res
        }
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

  private def buildSignMessage(
      dlcDb: DLCDb,
      sigDbs: Vector[DLCCETSignaturesDb],
      offerRefundSig: PartialSignature,
      fundingInputDbs: Vector[DLCFundingInputDb]): DLCSign = {
    {
      //if we don't have an acceptOpt because we don't have CET sigs
      //how are we getting them here?
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
                    throw new RuntimeException("Script witness cannot be empty")
                  case witness: ScriptWitnessV0 =>
                    (input.outPoint, witness)
                }
              case None =>
                throw new RuntimeException("Must be segwit")
            }
          }

      DLCSign(cetSigs,
              offerRefundSig,
              FundingSignatures(fundingSigs),
              contractId)
    }
  }
}
