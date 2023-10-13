package org.bitcoins.wallet.callback

import org.bitcoins.core.api.Callback
import org.bitcoins.core.api.wallet.db.SpendingInfoDb

trait OnReservedUtxos extends Callback[Vector[SpendingInfoDb]]
