package org.bitcoins.rpc.client


import org.bitcoins.rpc.jsonmodels.{GetNetworkInfoResult, Node, NodeBan}
import java.net.URI

import play.api.libs.json.{JsBoolean, JsString}

import scala.concurrent.Future

trait P2PCalls extends Client{

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

  def setNetworkActive(activate: Boolean): Future[Unit] = {
    bitcoindCall[Unit]("setnetworkactive", List(JsBoolean(activate)))
  }
}
