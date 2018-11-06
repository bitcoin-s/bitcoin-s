package org.bitcoins.rpc.client


import org.bitcoins.rpc.jsonmodels.{GetNetworkInfoResult, Node, NodeBan}
import org.bitcoins.rpc.serializers.JsonSerializers._
import org.bitcoins.rpc.serializers.JsonReaders._
import java.net.URI

import play.api.libs.json.{JsBoolean, JsNumber, JsString}

import scala.concurrent.Future

protected trait P2PCalls extends Client with BitcoindCall {

  def addNode(address: URI, command: String): Future[Unit] = {
    bitcoindCall[Unit](
      "addnode",
      List(JsString(address.getAuthority), JsString(command)))
  }

  def clearBanned(): Future[Unit] = {
    bitcoindCall[Unit]("clearbanned")
  }

  def disconnectNode(address: URI): Future[Unit] = {
    bitcoindCall[Unit]("disconnectnode", List(JsString(address.getAuthority)))
  }

  private def getAddedNodeInfo(node: Option[URI]): Future[Vector[Node]] = {
    val params =
      if (node.isEmpty) {
        List.empty
      } else {
        List(JsString(node.get.getAuthority))
      }
    bitcoindCall[Vector[Node]]("getaddednodeinfo", params)
  }

  def getAddedNodeInfo: Future[Vector[Node]] = getAddedNodeInfo(None)

  def getAddedNodeInfo(node: URI): Future[Vector[Node]] =
    getAddedNodeInfo(Some(node))

  def getConnectionCount: Future[Int] = {
    bitcoindCall[Int]("getconnectioncount")
  }

  def getNetworkInfo: Future[GetNetworkInfoResult] = {
    bitcoindCall[GetNetworkInfoResult]("getnetworkinfo")
  }

  def listBanned: Future[Vector[NodeBan]] = {
    bitcoindCall[Vector[NodeBan]]("listbanned")
  }

  def ping(): Future[Unit] = {
    bitcoindCall[Unit]("ping")
  }

  def setBan(
              address: URI,
              command: String,
              banTime: Int = 86400,
              absolute: Boolean = false): Future[Unit] = {
    bitcoindCall[Unit](
      "setban",
      List(
        JsString(address.getAuthority),
        JsString(command),
        JsNumber(banTime),
        JsBoolean(absolute)))
  }

  def setNetworkActive(activate: Boolean): Future[Unit] = {
    bitcoindCall[Unit]("setnetworkactive", List(JsBoolean(activate)))
  }
}
