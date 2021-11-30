package org.bitcoins.dlc.wallet.util

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
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
import slick.dbio.{DBIO, DBIOAction, Effect, NoStream}

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

  /** Builds an offer in our database, adds relevant information to the global table,
    * contract data, announcements, funding inputs, and the actual offer itself
    */
  def buildCreateOfferAction(
      dlcDb: DLCDb,
      contractDataDb: DLCContractDataDb,
      dlcAnnouncementDbs: Vector[DLCAnnouncementDb],
      dlcInputs: Vector[DLCFundingInputDb],
      dlcOfferDb: DLCOfferDb)(implicit
      ec: ExecutionContext): DBIOAction[Unit, NoStream, Effect.Write] = {
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
      refundSigsDb: DLCRefundSigsDb)(implicit
      ec: ExecutionContext): DBIOAction[Unit, NoStream, Effect.Write] = {
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
    allActions
  }
}
