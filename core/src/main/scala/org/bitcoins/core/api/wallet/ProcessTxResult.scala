package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.wallet.db.SpendingInfoDb

case class ProcessTxResult(
    updatedIncoming: Vector[SpendingInfoDb],
    updatedOutgoing: Vector[SpendingInfoDb])
