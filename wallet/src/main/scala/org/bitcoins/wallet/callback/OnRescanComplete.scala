package org.bitcoins.wallet.callback

import org.bitcoins.core.api.Callback

/** Triggered when a rescan is */
trait OnRescanComplete extends Callback[String]
