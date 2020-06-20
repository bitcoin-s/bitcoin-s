package org.bitcoins.testkit.node.fixture

import org.bitcoins.node.{NeutrinoNode, Node, SpvNode}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient

/** Gives us a fixture that has a SPV node connected with the bitcoind instance */
trait NodeConnectedWithBitcoind {
  def node: Node
  def bitcoind: BitcoindRpcClient
}

case class SpvNodeConnectedWithBitcoind(
    node: SpvNode,
    bitcoind: BitcoindRpcClient)
    extends NodeConnectedWithBitcoind

case class SpvNodeConnectedWithBitcoindV19(
    node: SpvNode,
    bitcoind: BitcoindV19RpcClient)
    extends NodeConnectedWithBitcoind

case class NeutrinoNodeConnectedWithBitcoind(
    node: NeutrinoNode,
    bitcoind: BitcoindRpcClient)
    extends NodeConnectedWithBitcoind
