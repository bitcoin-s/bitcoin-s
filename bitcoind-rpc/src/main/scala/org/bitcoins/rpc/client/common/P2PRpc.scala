package org.bitcoins.rpc.client.common

import java.net.URI

import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.rpc.client.common.RpcOpts.{AddNodeArgument, SetBanCommand}
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.{JsBoolean, JsNumber, JsString}

import scala.concurrent.Future

/**
  * This trait defines functionality relating to how
  * Bitcoin Core connects to and selects its network peers.
  */
trait P2PRpc { self: Client =>

  def addNode(address: URI, command: AddNodeArgument): Future[Unit] = {
    bitcoindCall[Unit](
      "addnode",
      List(JsString(address.getAuthority), JsString(command.toString)))
  }

  def clearBanned(): Future[Unit] = {
    bitcoindCall[Unit]("clearbanned")
  }

  def disconnectNode(address: URI): Future[Unit] = {
    bitcoindCall[Unit]("disconnectnode", List(JsString(address.getAuthority)))
  }

  def getAddedNodeInfo: Future[Vector[Node]] = getAddedNodeInfo(None)

  private def getAddedNodeInfo(node: Option[URI]): Future[Vector[Node]] = {
    val params =
      if (node.isEmpty) {
        List.empty
      } else {
        List(JsString(node.get.getAuthority))
      }
    bitcoindCall[Vector[Node]]("getaddednodeinfo", params)
  }

  def getAddedNodeInfo(node: URI): Future[Vector[Node]] =
    getAddedNodeInfo(Some(node))

  def getConnectionCount: Future[Int] = {
    bitcoindCall[Int]("getconnectioncount")
  }

  def getNetTotals: Future[GetNetTotalsResult] = {
    bitcoindCall[GetNetTotalsResult]("getnettotals")
  }

  def getNetworkInfo: Future[GetNetworkInfoResult] = {
    bitcoindCall[GetNetworkInfoResult]("getnetworkinfo")
  }

  def getPeerInfo: Future[Vector[Peer]] = {
    bitcoindCall[Vector[Peer]]("getpeerinfo")
  }

  def listBanned: Future[Vector[NodeBan]] = {
    bitcoindCall[Vector[NodeBan]]("listbanned")
  }

  def setBan(
      address: URI,
      command: SetBanCommand,
      banTime: Int = 86400,
      absolute: Boolean = false): Future[Unit] = {
    bitcoindCall[Unit]("setban",
                       List(JsString(address.getAuthority),
                            JsString(command.toString),
                            JsNumber(banTime),
                            JsBoolean(absolute)))
  }

  def setNetworkActive(activate: Boolean): Future[Unit] = {
    bitcoindCall[Unit]("setnetworkactive", List(JsBoolean(activate)))
  }

  def submitBlock(block: Block): Future[Unit] = {
    bitcoindCall[Unit]("submitblock", List(JsString(block.hex)))

  }
}
