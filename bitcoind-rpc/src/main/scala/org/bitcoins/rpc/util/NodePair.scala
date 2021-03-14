package org.bitcoins.rpc.util

import org.bitcoins.rpc.client.common.BitcoindRpcClient

case class NodePair(node1: BitcoindRpcClient, node2: BitcoindRpcClient) {
  val toVector: Vector[BitcoindRpcClient] = Vector(node1, node2)
}

object NodePair {

  def fromTuple(pair: (BitcoindRpcClient, BitcoindRpcClient)): NodePair = {
    NodePair(pair._1, pair._2)
  }
}
