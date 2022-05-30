package org.bitcoins.testkit.node.fixture

import org.bitcoins.node.{NeutrinoNode, Node}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v22.BitcoindV22RpcClient

/** Gives us a fixture that has a Neutrino node connected with the bitcoind instance */
trait NodeConnectedWithBitcoind {
  def node: Node
  def bitcoind: BitcoindRpcClient
}

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

case class NeutrinoNodeConnectedWithBitcoindV22(
    node: NeutrinoNode,
    bitcoind: BitcoindV22RpcClient)
    extends NodeConnectedWithBitcoind
