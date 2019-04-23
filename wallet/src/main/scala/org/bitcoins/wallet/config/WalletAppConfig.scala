package org.bitcoins.wallet.config

import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.db.AppConfig
import org.bitcoins.wallet.db.WalletDbConfig

case class WalletAppConfig(dbConfig: WalletDbConfig, chain: ChainParams) extends AppConfig[WalletDbConfig]
