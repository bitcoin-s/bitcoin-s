package org.bitcoins.testkit.node

import org.bitcoins.core.api.wallet.HDWalletApi
import org.bitcoins.node.{NeutrinoNode, Node}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.node.fixture.{
  NeutrinoNodeConnectedWithBitcoind,
  NodeConnectedWithBitcoind
}

/** Creates
  * 1. a funded bitcoind wallet
  * 2. a funded bitcoin-s wallet
  * 3. a chain handler with the appropriate tables created
  * 4. a neutrino node that is connected to the bitcoin instance -- but not started!
  */
trait NodeFundedWalletBitcoind {
  def node: Node
  def wallet: HDWalletApi
  def bitcoindRpc: BitcoindRpcClient

  /** Helper method to convert from this to a [[NodeConnectedWithBitcoind]] */
  def toNodeConnectedWithBitcoind: NodeConnectedWithBitcoind
}

case class NeutrinoNodeFundedWalletBitcoind(
    node: NeutrinoNode,
    wallet: HDWalletApi,
    bitcoindRpc: BitcoindRpcClient)
    extends NodeFundedWalletBitcoind {

  override def toNodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoind = {
    NeutrinoNodeConnectedWithBitcoind(node, bitcoindRpc)
  }
}
