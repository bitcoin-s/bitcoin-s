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
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  SchnorrDigitalSignature,
  Sha256Digest
}
import org.bitcoins.db.SafeDatabase
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.dlc.wallet.models._
import org.bitcoins.wallet.internal.TransactionProcessing

import scala.concurrent._

/** Overrides TransactionProcessing from Wallet to add extra logic to
  * process transactions that could from our own DLC.
  */
private[bitcoins] trait DLCTransactionProcessing extends TransactionProcessing {
  self: DLCWallet =>
  private lazy val safeDatabase: SafeDatabase = dlcDAO.safeDatabase

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
        if (dlcDb.state != DLCState.RemoteClaimed) {
          val withState = dlcDb.updateState(DLCState.RemoteClaimed)
          for {
            withOutcomeOpt <- calculateAndSetOutcome(withState)
            dlc <- findDLC(dlcDb.dlcId)
            _ = dlcConfig.walletCallbacks.executeOnDLCStateChange(logger,
                                                                  dlc.get)
          } yield {
            withOutcomeOpt
          }
        } else {
          //if the state is RemoteClaimed... we don't want to
          //calculate and set outcome again
          Future.successful(Some(dlcDb))
        }
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
                  DLCState.Broadcasted | DLCState.Confirmed |
                  _: DLCState.AdaptorSigComputationState =>
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
  private def calculateAndSetOutcome(dlcDb: DLCDb): Future[Option[DLCDb]] = {
    if (dlcDb.state == DLCState.RemoteClaimed) {
      logger.info(
        s"Calculating RemotedClaimed outcome for dlcId=${dlcDb.dlcId.hex} closingTx=${dlcDb.closingTxIdOpt
          .map(_.hex)}")
      val dlcId = dlcDb.dlcId

      for {
        setupStateOpt <- dlcDataManagement.getDLCFundingData(dlcId,
                                                             txDAO =
                                                               transactionDAO)
        completeDbState = {
          setupStateOpt.get match {
            case c: CompleteSetupDLCDbState => c
            case offered: OfferedDbState =>
              sys.error(
                s"Cannot calculate and set outcome of dlc that is only offered, id=${offered.dlcDb.dlcId.hex}")
          }
        }
        (sigDbs, refundSigOpt) <- dlcDataManagement.getCetAndRefundSigs(dlcId)
        (announcements, announcementData, nonceDbs) <- dlcDataManagement
          .getDLCAnnouncementDbs(dlcId)

        cet <-
          transactionDAO
            .read(dlcDb.closingTxIdOpt.get)
            .map(_.get.transaction.asInstanceOf[WitnessTransaction])

        sigAndOutcome = recoverSigAndOutcomeForRemoteClaimed(
          completeDbState = completeDbState,
          cet = cet,
          sigDbs = sigDbs,
          refundSigsDbOpt = refundSigOpt)
        sig = sigAndOutcome._1
        outcome = sigAndOutcome._2
        oracleInfos = getOutcomeDbInfo(outcome)._2

        noncesByAnnouncement = nonceDbs
          .groupBy(_.announcementId)

        announcementsWithId = dlcDataManagement.getOracleAnnouncementsWithId(
          announcements,
          announcementData,
          nonceDbs)

        usedIds = {
          announcementsWithId
            .filter(t => oracleInfos.exists(_.announcement == t._1))
            .map(_._2)
        }

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
        updatedDlcDbSig = dlcDb.copy(aggregateSignatureOpt = Some(sig))
        //updates the aggregateSignatureOpt along with the state to RemoteClaimed
        updatedDlcDbA = dlcDAO.updateAction(updatedDlcDbSig)
        updateNonceA = oracleNonceDAO.updateAllAction(updatedNonces)
        updateAnnouncementA = dlcAnnouncementDAO.updateAllAction(
          updatedAnnouncements)
        actions = {
          for {
            updatedDlcDb <- updatedDlcDbA
            _ <- updateNonceA
            _ <- updateAnnouncementA
          } yield updatedDlcDb
        }
        updatedDlcDb <- safeDatabase.run(actions)
      } yield {
        logger.info(
          s"Done calculating RemoteClaimed outcome for dlcId=${dlcId.hex}")
        Some(updatedDlcDb)
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
              case _: DLCState.AdaptorSigComputationState =>
                val contractIdOpt = dlcDb.contractIdOpt.map(_.toHex)
                throw new IllegalStateException(
                  s"Cannot be settling a DLC when we are computing adaptor sigs! contractId=${contractIdOpt}")
              case DLCState.Confirmed | DLCState.Claimed |
                  DLCState.RemoteClaimed | DLCState.Refunded =>
                dlcDb
            }
          }

          _ <- dlcDAO.updateAll(updated)
          dlcIds = updated.map(_.dlcId).distinct
          isRescanning <- isRescanning()
          _ <- sendWsDLCStateChange(dlcIds, isRescanning)
        } yield {
          res
        }
      }
  }

  /** Sends out a websocket event for the given dlcIds since their [[DLCState]] changed
    * @param dlcIds the dlcIds that had their status change
    * @param isRescanning if the wallet is rescanning or not, we don't want to send out events if the wallet is rescanning
    */
  private def sendWsDLCStateChange(
      dlcIds: Vector[Sha256Digest],
      isRescanning: Boolean): Future[Unit] = {
    if (isRescanning) {
      //don't send ws events if we are rescanning the wallet
      Future.unit
    } else {
      val updatedDlcDbsF = Future.sequence(dlcIds.map(findDLC))
      val sendF = updatedDlcDbsF.flatMap { updatedDlcDbs =>
        Future.sequence {
          updatedDlcDbs.map(u =>
            dlcConfig.walletCallbacks.executeOnDLCStateChange(logger, u.get))
        }
      }
      sendF.map(_ => ())
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

  /** Recovers the aggregate signature and oracle outcome used
    * to claim the DLC. Remember, this flow is for [[DLCState.RemoteClaimed]]
    * so we do not necessarily have access to what the [[OracleAttestment]] is
    */
  private def recoverSigAndOutcomeForRemoteClaimed(
      completeDbState: CompleteSetupDLCDbState,
      cet: WitnessTransaction,
      sigDbs: Vector[DLCCETSignaturesDb],
      refundSigsDbOpt: Option[DLCRefundSigsDb]): (
      SchnorrDigitalSignature,
      OracleOutcome) = {
    val dlcDb = completeDbState.dlcDb
    val dlcId = dlcDb.dlcId
    val isInit = dlcDb.isInitiator

    val offer = completeDbState.offer

    val acceptOpt = completeDbState.acceptOpt
    require(
      acceptOpt.isDefined,
      s"Accept message must still have CET signatures to recover an outcome on chain, dlcId=${dlcId.hex}")
    val accept = acceptOpt.get

    val fundingInputDbs = completeDbState.allFundingInputs
    val offerRefundSigOpt = refundSigsDbOpt.flatMap(_.initiatorSig)

    val signOpt: Option[DLCSign] = offerRefundSigOpt.map { refundSig =>
      buildSignMessage(
        dlcDb = dlcDb,
        sigDbs = sigDbs,
        offerRefundSig = refundSig,
        fundingInputDbs = fundingInputDbs
      )
    }

    require(
      signOpt.isDefined,
      s"Can only recompute outcome if signMsg is defined, dlcId=${dlcId.hex}")
    val sign = signOpt.get

    val sigsAndOutcomeOpt = DLCStatus.calculateOutcomeAndSig(isInitiator =
                                                               isInit,
                                                             offer = offer,
                                                             accept = accept,
                                                             sign = sign,
                                                             cet = cet)

    require(
      sigsAndOutcomeOpt.isDefined,
      s"We must be able to calculate an outcome from a CET broadcast on   chain, dlcId=${offer.dlcId.hex} cet.txIdBE=${cet.txIdBE.hex}"
    )

    sigsAndOutcomeOpt.get
  }
}
