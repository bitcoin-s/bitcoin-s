package org.bitcoins.core.api.dlc.wallet.db

import java.net.InetSocketAddress

case class DLCContactDb(alias: String, address: InetSocketAddress, memo: String)
