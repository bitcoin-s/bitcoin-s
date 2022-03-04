package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.protocol.dlc.models.DLCMessage.DLCAcceptWithoutSigs
import org.bitcoins.core.protocol.dlc.models.DLCPublicKeys

case class InitializedAccept(
    dlc: DLCDb,
    offerDb: DLCOfferDb,
    acceptDb: DLCAcceptDb,
    fundingInputsDb: Vector[DLCFundingInputDb],
    pubKeys: DLCPublicKeys,
    contractDataDb: DLCContractDataDb,
    acceptWithoutSigs: DLCAcceptWithoutSigs) {
  require(dlc.dlcId == offerDb.dlcId,
          s"dlc.dlcId=${dlc.dlcId} offerDb.dlcId=${offerDb.dlcId}")
  require(dlc.dlcId == acceptDb.dlcId,
          s"dlc.dlcId=${dlc.dlcId} offerDb.dlcId=${acceptDb.dlcId}")
  require(dlc.dlcId == contractDataDb.dlcId,
          s"dlc.dlcId=${dlc.dlcId} offerDb.dlcId=${contractDataDb.dlcId}")
  require(
    pubKeys == acceptWithoutSigs.pubKeys,
    s"pubKeys=${pubKeys} acceptWithoutSigs.pubKeys=${acceptWithoutSigs.pubKeys}")
}
