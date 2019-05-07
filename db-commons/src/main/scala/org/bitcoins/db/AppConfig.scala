package org.bitcoins.db

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.util.BitcoinSLogger


trait AppConfig[C <: DbConfig] extends BitcoinSLogger {
  def dbConfig: C

  def chain: ChainParams = dbConfig.networkDb.chain

  def network: NetworkParameters = chain.network
}