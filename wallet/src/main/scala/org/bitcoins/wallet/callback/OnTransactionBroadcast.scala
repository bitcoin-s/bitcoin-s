package org.bitcoins.wallet.callback

import org.bitcoins.core.api.Callback
import org.bitcoins.core.protocol.transaction.Transaction

trait OnTransactionBroadcast extends Callback[Transaction]
