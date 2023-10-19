package org.bitcoins.wallet.callback

import org.bitcoins.core.api.Callback
import org.bitcoins.core.protocol.BitcoinAddress

trait OnNewAddressGenerated extends Callback[BitcoinAddress]
