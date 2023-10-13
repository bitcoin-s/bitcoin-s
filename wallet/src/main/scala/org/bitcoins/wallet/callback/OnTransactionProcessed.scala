package org.bitcoins.wallet.callback

import org.bitcoins.core.api.Callback
import org.bitcoins.core.protocol.transaction.Transaction

/** Callback for handling a processed transaction */
trait OnTransactionProcessed extends Callback[Transaction]
