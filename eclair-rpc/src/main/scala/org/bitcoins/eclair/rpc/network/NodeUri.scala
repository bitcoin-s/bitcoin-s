package org.bitcoins.eclair.rpc.network

import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.crypto.StringFactory

import scala.util.{Failure, Success, Try}

case class NodeUri(nodeId: NodeId, host: String, port: Int) {
  override def toString = s"$nodeId@$host:$port"
}

object NodeUri extends StringFactory[NodeUri] {

  private val defaultPort = ":9735"

  override def fromStringT(uri: String): Try[NodeUri] = {
    val patternWithPort = """(\w+)@([\w.]+(\w+)):(\d{2,5})""".r

    val isUriWithPort = patternWithPort.findFirstIn(uri)

    val nodeUriT = isUriWithPort match {
      case Some(withPort) =>
        Success(parse(withPort))
      case None =>
        Failure(
          new IllegalArgumentException(s"Failed to parse $uri to a NodeUri"))
    }
    nodeUriT
  }

  override def fromString(string: String): NodeUri = {
    fromStringT(string) match {
      case Success(uri) => uri
      case Failure(exn) => throw exn
    }
  }

  def fromStringNoPort(uri: String): Try[NodeUri] = {
    fromStringT(uri + defaultPort)
  }

  /**
    * Assumes format is [nodeId]@[host]:[port]
    */
  private def parse(validUri: String): NodeUri = {
    //key is 33 bytes in size
    val (key: String, rest: String) = validUri.splitAt(66)

    val (host, port) = rest.splitAt(rest.size - 5)

    val nodeId = NodeId.fromHex(key)

    NodeUri(nodeId, host.tail, port.tail.toInt)
  }
}
