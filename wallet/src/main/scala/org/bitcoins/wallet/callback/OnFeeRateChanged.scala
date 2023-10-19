package org.bitcoins.wallet.callback

import org.bitcoins.core.api.Callback
import org.bitcoins.core.wallet.fee.FeeUnit

trait OnFeeRateChanged extends Callback[FeeUnit]
