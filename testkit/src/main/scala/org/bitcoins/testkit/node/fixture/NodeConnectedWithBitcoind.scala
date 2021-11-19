package org.bitcoins.testkit.node.fixture

import org.bitcoins.node.{NeutrinoNode, Node, SpvNode}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.rpc.client.v21.BitcoindV21RpcClient
import org.bitcoins.rpc.client.v22.BitcoindV22RpcClient

/** Gives us a fixture that has a SPV node connected with the bitcoind instance */
trait NodeConnectedWithBitcoind {
  def node: Node
  def bitcoind: BitcoindRpcClient
}

case class SpvNodeConnectedWithBitcoind(
    node: SpvNode,
    bitcoind: BitcoindRpcClient)
    extends NodeConnectedWithBitcoind

case class SpvNodeConnectedWithBitcoindV22(
    node: SpvNode,
    bitcoind: BitcoindV22RpcClient)
    extends NodeConnectedWithBitcoind

case class SpvNodeConnectedWithBitcoindV21(
    node: SpvNode,
    bitcoind: BitcoindV21RpcClient)
    extends NodeConnectedWithBitcoind

case class SpvNodeConnectedWithBitcoindV19(
    node: SpvNode,
    bitcoind: BitcoindV19RpcClient)
    extends NodeConnectedWithBitcoind

case class NeutrinoNodeConnectedWithBitcoind(
    node: NeutrinoNode,
    bitcoind: BitcoindRpcClient)
    extends NodeConnectedWithBitcoind

trait NodeConnectedWithBitcoinds {
  def node: Node
  def bitcoinds: Vector[BitcoindRpcClient]
}

case class NeutrinoNodeConnectedWithBitcoinds(
    node: NeutrinoNode,
    bitcoinds: Vector[BitcoindRpcClient]
) extends NodeConnectedWithBitcoinds
