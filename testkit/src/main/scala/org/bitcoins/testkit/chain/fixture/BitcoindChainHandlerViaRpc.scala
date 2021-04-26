package org.bitcoins.testkit.chain.fixture

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient

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

case class BitcoindV19ChainHandler(
    override val bitcoindRpc: BitcoindV19RpcClient,
    chainHandler: ChainHandler)
    extends BitcoindChainHandlerViaRpc
