package org.bitcoins.rpc.util

import org.bitcoins.rpc.client.common.BitcoindRpcClient

case class NodePair[T <: BitcoindRpcClient](node1: T, node2: T) {
  val toVector: Vector[T] = Vector(node1, node2)
}

object NodePair {

  def fromTuple[T <: BitcoindRpcClient](pair: (T, T)): NodePair[T] = {
    NodePair(pair._1, pair._2)
  }
}
