package org.bitcoins.testkit.node.fixture

import org.bitcoins.node.{NeutrinoNode, Node, SpvNode}
import org.bitcoins.rpc.client.common.BitcoindRpcClient

/** Gives us a fixture that has a SPV node connected with the bitcoind instance */
case class NodeConnectedWithBitcoind(node: Node, bitcoind: BitcoindRpcClient) {
  def spvNode: SpvNode = node.asInstanceOf[SpvNode]
  def neutrinoNode: NeutrinoNode = node.asInstanceOf[NeutrinoNode]
}
