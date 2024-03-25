package org.bitcoins.testkit.chain.fixture

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.rpc.client.common.BitcoindRpcClient

sealed trait BitcoindChainHandlerViaRpc {
  def bitcoindRpc: BitcoindRpcClient
  def chainHandler: ChainHandler
}

/** Represents a bitcoind instance paired with a chain handler via rpc
  * This is useful for when the bitcoind version doesn't matter, you
  * just need a generic [[BitcoindRpcClient]]
  */
case class BitcoindBaseVersionChainHandlerViaRpc(
    bitcoindRpc: BitcoindRpcClient,
    chainHandler: ChainHandler)
    extends BitcoindChainHandlerViaRpc
