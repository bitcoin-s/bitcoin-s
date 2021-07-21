package org.bitcoins.dlc.wallet.accounting

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.dlc.wallet.models.{DLCAcceptDb, DLCOfferDb}

case class DLCAccountingDbs(
    dlcDb: DLCDb,
    offerDb: DLCOfferDb,
    acceptDb: DLCAcceptDb,
    closingTx: Transaction) {
  require(
    dlcDb.dlcId == offerDb.dlcId,
    s"dlcDb.dlcId not equal to offerDb.dlcId, got dlcDb.dlcId=${dlcDb.dlcId} offerDb.dlcId=${offerDb.dlcId}")
  require(
    offerDb.dlcId == acceptDb.dlcId,
    s"OfferDb and acceptDb not the same offerDb.dlcId=${offerDb.dlcId}, acceptDb.dlcId=${acceptDb.dlcId}")
}
