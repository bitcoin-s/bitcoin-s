package org.bitcoins.db

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.blockchain.ChainParams

case class AppConfig(dbConfig: DbConfig, chain: ChainParams) {
  val network: NetworkParameters = chain.network
}
