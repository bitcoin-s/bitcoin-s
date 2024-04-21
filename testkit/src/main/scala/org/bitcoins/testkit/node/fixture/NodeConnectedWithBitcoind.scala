package org.bitcoins.testkit.node.fixture

import org.bitcoins.node.{NeutrinoNode, Node}
import org.bitcoins.rpc.client.common.BitcoindRpcClient

/** Gives us a fixture that has a Neutrino node connected with the bitcoind
  * instance
  */
trait NodeConnectedWithBitcoind {
  def node: Node
  def bitcoind: BitcoindRpcClient
}

case class NeutrinoNodeConnectedWithBitcoind(
    node: NeutrinoNode,
    bitcoind: BitcoindRpcClient
) extends NodeConnectedWithBitcoind

trait NodeConnectedWithBitcoinds {
  def node: Node
  def bitcoinds: Vector[BitcoindRpcClient]
}

case class NeutrinoNodeConnectedWithBitcoinds(
    node: NeutrinoNode,
    bitcoinds: Vector[BitcoindRpcClient]
) extends NodeConnectedWithBitcoinds

case class NeutrinoNodeNotConnectedWithBitcoinds(
    node: NeutrinoNode,
    bitcoinds: Vector[BitcoindRpcClient]
)
