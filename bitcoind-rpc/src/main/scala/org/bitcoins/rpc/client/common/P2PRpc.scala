package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{
  AddNodeArgument,
  SetBanCommand
}
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.protocol.blockchain.Block
import play.api.libs.json.{JsBoolean, JsNumber, JsString}

import java.net.URI
import scala.concurrent.Future

/** This trait defines functionality relating to how Bitcoin Core connects to
  * and selects its network peers.
  */
trait P2PRpc { self: Client =>

  def addNode(
      address: URI,
      command: AddNodeArgument,
      v2transport: Boolean = true): Future[Unit] = {
    bitcoindCall[Unit](
      "addnode",
      List(JsString(address.getAuthority),
           JsString(command.toString),
           JsBoolean(v2transport))
    )
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
    bitcoindCall[GetNetworkInfoResultPostV21]("getnetworkinfo")
      .recoverWith { case _ =>
        bitcoindCall[GetNetworkInfoResultPreV21]("getnetworkinfo")
      }
  }

  def getPeerInfo: Future[Vector[PeerInfoResponseV25]] = {
    bitcoindCall[Vector[PeerInfoResponseV25]]("getpeerinfo")
  }

  def listBanned: Future[Vector[NodeBan]] = {
    bitcoindCall[Vector[NodeBanPostV22]]("listbanned")
  }

  def setBan(
      address: URI,
      command: SetBanCommand,
      banTime: Int = 86400,
      absolute: Boolean = false
  ): Future[Unit] = {
    bitcoindCall[Unit](
      "setban",
      List(
        JsString(address.getAuthority),
        JsString(command.toString),
        JsNumber(banTime),
        JsBoolean(absolute)
      )
    )
  }

  def setNetworkActive(activate: Boolean): Future[Unit] = {
    bitcoindCall[Unit]("setnetworkactive", List(JsBoolean(activate)))
  }

  def submitBlock(block: Block): Future[Unit] = {
    bitcoindCall[Unit]("submitblock", List(JsString(block.hex)))
  }

  def getAddrManInfo(): Future[GetAddrmanInfoResponse] = {
    bitcoindCall[GetAddrmanInfoResponse]("getaddrmaninfo")(
      JsonSerializers.getAddrmanInfoResponseReads)
  }
}
