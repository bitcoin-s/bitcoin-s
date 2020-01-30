package org.bitcoins.dlc

import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfoFull

/** Contains all DLC transactions and the BitcoinUTXOSpendingInfos they use. */
case class DLCOutcome(
    fundingTx: Transaction,
    cet: Transaction,
    closingTx: Transaction,
    cetSpendingInfo: BitcoinUTXOSpendingInfoFull
)

case class CooperativeDLCOutcome(fundingTx: Transaction, closingTx: Transaction)
