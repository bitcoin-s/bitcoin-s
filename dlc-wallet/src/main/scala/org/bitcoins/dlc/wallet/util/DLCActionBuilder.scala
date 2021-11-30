package org.bitcoins.dlc.wallet.util

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.dlc.wallet.models.{
  DLCAcceptDAO,
  DLCAcceptDb,
  DLCAnnouncementDAO,
  DLCAnnouncementDb,
  DLCCETSignaturesDAO,
  DLCCETSignaturesDb,
  DLCContractDataDAO,
  DLCContractDataDb,
  DLCDAO,
  DLCFundingInputDAO,
  DLCFundingInputDb,
  DLCOfferDAO,
  DLCOfferDb,
  DLCRefundSigsDAO,
  DLCRefundSigsDb
}

import scala.concurrent.ExecutionContext

/** Utility class to help build actions to insert things into our DLC tables */
case class DLCActionBuilder(
    dlcDAO: DLCDAO,
    contractDataDAO: DLCContractDataDAO,
    dlcAnnouncementDAO: DLCAnnouncementDAO,
    dlcInputsDAO: DLCFundingInputDAO,
    dlcOfferDAO: DLCOfferDAO,
    dlcAcceptDAO: DLCAcceptDAO,
    dlcSigsDAO: DLCCETSignaturesDAO,
    dlcRefundSigDAO: DLCRefundSigsDAO) {

  //idk if it matters which profile api i import, but i need access to transactionally
  import dlcDAO.profile.api._

  /** Builds an offer in our database, adds relevant information to the global table,
    * contract data, announcements, funding inputs, and the actual offer itself
    */
  def buildCreateOfferAction(
      dlcDb: DLCDb,
      contractDataDb: DLCContractDataDb,
      dlcAnnouncementDbs: Vector[DLCAnnouncementDb],
      dlcInputs: Vector[DLCFundingInputDb],
      dlcOfferDb: DLCOfferDb)(implicit ec: ExecutionContext): DBIOAction[
    Unit,
    NoStream,
    Effect.Write with Effect.Transactional] = {
    val globalAction = dlcDAO.createAction(dlcDb)
    val contractAction = contractDataDAO.createAction(contractDataDb)
    val announcementAction =
      dlcAnnouncementDAO.createAllAction(dlcAnnouncementDbs)
    val inputsAction = dlcInputsDAO.createAllAction(dlcInputs)
    val offerAction = dlcOfferDAO.createAction(dlcOfferDb)
    val actions = Vector(globalAction,
                         contractAction,
                         announcementAction,
                         inputsAction,
                         offerAction)
    val allActions = DBIO
      .sequence(actions)
      .map(_ => ())
      .transactionally
    allActions
  }

  /** Builds an accept in our database, adds relevant information to the
    * offer table, accept table, cet sigs table, inputs table, and refund table
    */
  def buildCreateAcceptAction(
      dlcOfferDb: DLCOfferDb,
      dlcAcceptDb: DLCAcceptDb,
      offerInputs: Vector[DLCFundingInputDb],
      acceptInputs: Vector[DLCFundingInputDb],
      cetSigsDb: Vector[DLCCETSignaturesDb],
      refundSigsDb: DLCRefundSigsDb)(implicit ec: ExecutionContext): DBIOAction[
    Unit,
    NoStream,
    Effect.Write with Effect.Transactional] = {
    val inputAction = dlcInputsDAO.createAllAction(offerInputs ++ acceptInputs)
    val offerAction = dlcOfferDAO.createAction(dlcOfferDb)
    val acceptAction = dlcAcceptDAO.createAction(dlcAcceptDb)
    val sigsAction = dlcSigsDAO.createAllAction(cetSigsDb)
    val refundSigAction = dlcRefundSigDAO.createAction(refundSigsDb)
    val actions = Vector(inputAction,
                         offerAction,
                         acceptAction,
                         sigsAction,
                         refundSigAction)

    val allActions = DBIO
      .sequence(actions)
      .map(_ => ())
      .transactionally
    allActions
  }

  /** Creates the action to delete the given dlc from our database.
    * This removes references to the dlc in our various tables
    */
  def deleteDLCAction(dlcId: Sha256Digest)(implicit
      ec: ExecutionContext): DBIOAction[
    Unit,
    NoStream,
    Effect.Write with Effect.Transactional] = {
    val deleteSigA = dlcSigsDAO.deleteByDLCIdAction(dlcId)
    val deleteRefundSigA = dlcRefundSigDAO.deleteByDLCIdAction(dlcId)
    val deleteInputSigA = dlcInputsDAO.deleteByDLCIdAction(dlcId)
    val deleteAcceptA = dlcAcceptDAO.deleteByDLCIdAction(dlcId)
    val deleteOfferA = dlcOfferDAO.deleteByDLCIdAction(dlcId)
    val deleteContractDataA = contractDataDAO.deleteByDLCIdAction(dlcId)
    val deleteAnnouncementDataA = dlcAnnouncementDAO.deleteByDLCIdAction(dlcId)
    val deleteDlcA = dlcDAO.deleteByDLCIdAction(dlcId)

    val action = for {
      _ <- deleteSigA
      _ <- deleteRefundSigA
      _ <- deleteInputSigA
      _ <- deleteAcceptA
      _ <- deleteOfferA
      _ <- deleteContractDataA
      _ <- deleteAnnouncementDataA
      _ <- deleteDlcA
    } yield ()

    action.transactionally
  }

  /** Retrieves a DBIOAction that fetches the global dlc db,
    * the contract, the offer, and funding inputs
    */
  def getDLCOfferDataAction(dlcId: Sha256Digest)(implicit
      ec: ExecutionContext): DBIOAction[
    (
        Option[DLCDb],
        Option[DLCContractDataDb],
        Option[DLCOfferDb],
        Vector[DLCFundingInputDb]),
    NoStream,
    Effect.Read] = {
    val dlcDbAction = dlcDAO.findByDLCIdAction(dlcId)
    val contractDataAction = contractDataDAO.findByDLCIdAction(dlcId)
    val dlcOfferAction = dlcOfferDAO.findByDLCIdAction(dlcId)
    val fundingInputsAction = dlcInputsDAO.findByDLCIdAction(dlcId)
    val combined = for {
      dlcDb <- dlcDbAction
      contractData <- contractDataAction
      offer <- dlcOfferAction
      inputs <- fundingInputsAction
    } yield (dlcDb, contractData, offer, inputs)

    combined
  }
}
