package org.bitcoins.db

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.blockchain.ChainParams


trait AppConfig[C <: DbConfig] {
  def dbConfig: C

  def chain: ChainParams

  def network: NetworkParameters = chain.network
}