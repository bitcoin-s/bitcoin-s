package org.bitcoins.dlc.wallet.accounting

import org.bitcoins.core.dlc.accounting.DLCAccounting
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.dlc.wallet.models.{DLCAcceptDb, DLCDb, DLCOfferDb}

object AccountingUtil {

  /** Calculates the profit and loss for the given dlc */
  def calculatePnl(
      dlcDb: DLCDb,
      offerDb: DLCOfferDb,
      acceptDb: DLCAcceptDb,
      closingTx: Transaction): DLCAccounting = {
    val (myCollateral, theirCollateral, myPayoutAddress, theirPayoutAddress) = {
      if (dlcDb.isInitiator) {
        val myCollateral = offerDb.collateral
        val theirCollateral = acceptDb.collateral
        val myPayoutAddress = offerDb.payoutAddress
        val theirPayoutAddress = acceptDb.payoutAddress
        (myCollateral, theirCollateral, myPayoutAddress, theirPayoutAddress)

      } else {
        val myCollateral = acceptDb.collateral
        val theirCollateral = offerDb.collateral
        val myPayoutAddress = acceptDb.payoutAddress
        val theirPayoutAddress = offerDb.payoutAddress
        (myCollateral, theirCollateral, myPayoutAddress, theirPayoutAddress)
      }
    }

    val myPayout = closingTx.outputs
      .filter(_.scriptPubKey == myPayoutAddress.scriptPubKey)
      .map(_.value)
      .sum
    val theirPayout = closingTx.outputs
      .filter(_.scriptPubKey == theirPayoutAddress.scriptPubKey)
      .map(_.value)
      .sum
    DLCAccounting(
      dlcId = dlcDb.dlcId,
      myCollateral = myCollateral,
      theirCollateral = theirCollateral,
      myPayout = myPayout,
      theirPayout = theirPayout
    )
  }
}
