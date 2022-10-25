package org.bitcoins.testkit.chain.fixture

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.{V19BlockFilterRpc}

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

case class BitcoindBlockFilterRpcChainHandler(
    override val bitcoindRpc: BitcoindRpcClient with V19BlockFilterRpc,
    chainHandler: ChainHandler)
    extends BitcoindChainHandlerViaRpc
