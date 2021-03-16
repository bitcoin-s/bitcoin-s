package org.bitcoins.rpc.util

import org.bitcoins.rpc.client.common.BitcoindRpcClient

case class NodeTriple[T <: BitcoindRpcClient](node1: T, node2: T, node3: T) {
  val toVector: Vector[T] = Vector(node1, node2, node3)
}

object NodeTriple {

  def fromTuple[T <: BitcoindRpcClient](nodes: (T, T, T)): NodeTriple[T] = {
    NodeTriple(nodes._1, nodes._2, nodes._3)
  }
}
