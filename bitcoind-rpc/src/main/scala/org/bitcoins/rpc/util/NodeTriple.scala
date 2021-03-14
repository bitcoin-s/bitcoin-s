package org.bitcoins.rpc.util

import org.bitcoins.rpc.client.common.BitcoindRpcClient

case class NodeTriple(
    node1: BitcoindRpcClient,
    node2: BitcoindRpcClient,
    node3: BitcoindRpcClient) {
  val toVector: Vector[BitcoindRpcClient] = Vector(node1, node2, node3)
}

object NodeTriple {

  def fromTuple(
      nodes: (
          BitcoindRpcClient,
          BitcoindRpcClient,
          BitcoindRpcClient)): NodeTriple = {
    NodeTriple(nodes._1, nodes._2, nodes._3)
  }
}
