package org.bitcoins.wallet.callback

import org.bitcoins.core.api.Callback
import org.bitcoins.core.protocol.blockchain.Block

trait OnBlockProcessed extends Callback[Block]
