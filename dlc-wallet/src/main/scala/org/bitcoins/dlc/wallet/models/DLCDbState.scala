package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.api.wallet.db.TransactionDb
import org.bitcoins.core.protocol.dlc.models.DLCMessage.{
  DLCAccept,
  DLCAcceptWithoutSigs,
  DLCOffer
}
import org.bitcoins.core.protocol.dlc.models.DLCState.{
  ClosedState,
  InProgressState
}
import org.bitcoins.core.protocol.dlc.models.{
  CETSignatures,
  ContractInfo,
  DLCFundingInput,
  DLCState
}
import org.bitcoins.dlc.wallet.util.DLCTxUtil

/** Super trait to represent the state of a DLC in the database */
sealed trait DLCDbState {
  def dlcDb: DLCDb
  def contractDataDb: DLCContractDataDb
  def contractInfo: ContractInfo
  def offerFundingInputsDb: Vector[DLCFundingInputDb]
  def offerDb: DLCOfferDb
  def offerPrevTxs: Vector[TransactionDb]

  def offerFundingInputs: Vector[DLCFundingInput] = {
    DLCTxUtil.matchPrevTxsWithInputs(offerFundingInputsDb, offerPrevTxs)
  }

  def offer: DLCOffer = {
    offerDb.toDLCOffer(contractInfo = contractInfo,
                       fundingInputs = offerFundingInputs,
                       dlcDb = dlcDb,
                       contractDataDb = contractDataDb)
  }

  def state: DLCState
}

/** Represents a DLC in the database that
  * has not had its funding transaction published.
  * This means we are still setting up the DLC
  */
sealed trait DLCSetupDbState extends DLCDbState {
  override def state: InProgressState
}

/** Represents a DLC in the database that has
  * been fully setup and is in progress
  */
sealed trait DLCClosedDbState extends DLCDbState

case class OfferedDbState(
    dlcDb: DLCDb,
    contractDataDb: DLCContractDataDb,
    contractInfo: ContractInfo,
    offerDb: DLCOfferDb,
    offerFundingInputsDb: Vector[DLCFundingInputDb],
    offerPrevTxs: Vector[TransactionDb])
    extends DLCSetupDbState {
  //require(dlcDb.state == DLCState.Offered,
  //        s"OfferedDbState requires state offered, got=${dlcDb.state}")

  override val state: DLCState.Offered.type = DLCState.Offered

  /** Converts a [[OfferedDbState]] to an [[AcceptDbState]]
    * @param acceptDb
    * @param acceptFundingInputsDb
    * @param acceptPrevTxsDb
    * @param cetSignaturesOpt the cet signatures, if we haven't pruned them from the database
    */
  def toAcceptDb(
      acceptDb: DLCAcceptDb,
      acceptFundingInputsDb: Vector[DLCFundingInputDb],
      acceptPrevTxsDb: Vector[TransactionDb],
      cetSignaturesOpt: Option[CETSignatures],
      refundSigDb: DLCRefundSigsDb): AcceptDbState = {
    AcceptDbState(
      dlcDb = dlcDb,
      contractDataDb = contractDataDb,
      contractInfo = contractInfo,
      offerDb = offerDb,
      acceptDb = acceptDb,
      offerFundingInputsDb = offerFundingInputsDb,
      offerPrevTxs = offerPrevTxs,
      acceptFundingInputsDb = acceptFundingInputsDb,
      acceptPrevTxs = acceptPrevTxsDb,
      cetSignaturesOpt = cetSignaturesOpt,
      refundSigDb = refundSigDb
    )
  }
}

case class AcceptDbState(
    dlcDb: DLCDb,
    contractDataDb: DLCContractDataDb,
    contractInfo: ContractInfo,
    offerDb: DLCOfferDb,
    acceptDb: DLCAcceptDb,
    offerFundingInputsDb: Vector[DLCFundingInputDb],
    offerPrevTxs: Vector[TransactionDb],
    acceptFundingInputsDb: Vector[DLCFundingInputDb],
    acceptPrevTxs: Vector[TransactionDb],
    cetSignaturesOpt: Option[CETSignatures],
    refundSigDb: DLCRefundSigsDb)
    extends DLCSetupDbState {
  //require(dlcDb.state == DLCState.Accepted,
  //        s"OfferedDbState requires state accepted, got=${dlcDb.state}")

  override val state: DLCState.Accepted.type = DLCState.Accepted

  val acceptFundingInputs: Vector[DLCFundingInput] = {
    DLCTxUtil.matchPrevTxsWithInputs(acceptFundingInputsDb, acceptPrevTxs)
  }

  val acceptWithoutSigs: DLCAcceptWithoutSigs = {
    acceptDb.toDLCAcceptWithoutSigs(
      tempContractId = dlcDb.tempContractId,
      fundingInputs = acceptFundingInputs
    )
  }

  val allFundingInputs: Vector[DLCFundingInputDb] =
    offerFundingInputsDb ++ acceptFundingInputsDb

  val remotePrevTxs: Vector[TransactionDb] = {
    if (dlcDb.isInitiator) acceptPrevTxs
    else offerPrevTxs
  }

  val localPrevTxs: Vector[TransactionDb] = {
    if (dlcDb.isInitiator) offerPrevTxs
    else acceptPrevTxs
  }

  /** Reconstructs the [[DLCAccept]] message if we have [[CETSignatures]]
    * in the database. If we don't have the signatures because we have pruned
    * them we return None as we can't reconstruct the message
    */
  def acceptOpt: Option[DLCAccept] = {
    cetSignaturesOpt.map { cetSignatures =>
      acceptDb.toDLCAccept(dlcDb.tempContractId,
                           acceptFundingInputs,
                           outcomeSigs = cetSignatures.outcomeSigs,
                           refundSig = refundSigDb.accepterSig)
    }
  }
}

case class ClosedDbStateWithCETSigs(
    dlcDb: DLCDb,
    contractDataDb: DLCContractDataDb,
    contractInfo: ContractInfo,
    offerDb: DLCOfferDb,
    acceptDb: DLCAcceptDb,
    offerFundingInputsDb: Vector[DLCFundingInputDb],
    offerPrevTxs: Vector[TransactionDb],
    acceptFundingInputsDb: Vector[DLCFundingInputDb],
    acceptPrevTxs: Vector[TransactionDb],
    refundSigsDb: DLCRefundSigsDb,
    cetSigs: Vector[DLCCETSignaturesDb]
) extends DLCClosedDbState {
  //require(
  //  dlcDb.state.isInstanceOf[DLCState.ClosedState],
  //  s"ClosedDbStateWithCETSigs dlc state must be closed, got=${dlcDb.state}")

  override val state: DLCState = dlcDb.state

  val allFundingInputs: Vector[DLCFundingInputDb] =
    offerFundingInputsDb ++ acceptFundingInputsDb
}

/** Sometimes we prune CET sigs from the database to save on disk space.
  * We need to handle this different than [[ClosedDbStateWithCETSigs]]
  */
case class ClosedDbStateNoCETSigs(
    dlcDb: DLCDb,
    contractDataDb: DLCContractDataDb,
    contractInfo: ContractInfo,
    offerDb: DLCOfferDb,
    acceptDb: DLCAcceptDb,
    offerFundingInputsDb: Vector[DLCFundingInputDb],
    offerPrevTxs: Vector[TransactionDb],
    acceptFundingInputsDb: Vector[DLCFundingInputDb],
    acceptPrevTxs: Vector[TransactionDb],
    refundSigsDb: DLCRefundSigsDb)
    extends DLCClosedDbState {
  //require(
  //  dlcDb.state.isInstanceOf[DLCState.ClosedState],
  //  s"ClosedDbStateWithCETSigs dlc state must be closed, got=${dlcDb.state}")

  override val state: ClosedState =
    dlcDb.state.asInstanceOf[DLCState.ClosedState]
}

object DLCClosedDbState {

  def fromSetupState(
      acceptState: AcceptDbState,
      cetSigsOpt: Option[Vector[DLCCETSignaturesDb]]): DLCClosedDbState = {
    cetSigsOpt match {
      case Some(cetSigs) =>
        ClosedDbStateWithCETSigs(
          acceptState.dlcDb,
          acceptState.contractDataDb,
          acceptState.contractInfo,
          acceptState.offerDb,
          acceptState.acceptDb,
          acceptState.offerFundingInputsDb,
          acceptState.offerPrevTxs,
          acceptState.acceptFundingInputsDb,
          acceptState.acceptPrevTxs,
          acceptState.refundSigDb,
          cetSigs
        )
      case None =>
        ClosedDbStateNoCETSigs(
          acceptState.dlcDb,
          acceptState.contractDataDb,
          acceptState.contractInfo,
          acceptState.offerDb,
          acceptState.acceptDb,
          acceptState.offerFundingInputsDb,
          acceptState.offerPrevTxs,
          acceptState.acceptFundingInputsDb,
          acceptState.acceptPrevTxs,
          acceptState.refundSigDb
        )
    }
  }
}
