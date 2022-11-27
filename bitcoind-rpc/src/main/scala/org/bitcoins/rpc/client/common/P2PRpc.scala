package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{
  AddNodeArgument,
  SetBanCommand
}
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.rpc.client.common.BitcoindVersion._
import play.api.libs.json.{JsBoolean, JsNumber, JsString}

import java.net.URI
import scala.concurrent.Future

/** This trait defines functionality relating to how
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
    bitcoindCall[GetNetworkInfoResultPostV21]("getnetworkinfo")
      .recoverWith { case _ =>
        bitcoindCall[GetNetworkInfoResultPreV21]("getnetworkinfo")
      }
  }

  def getPeerInfo: Future[Vector[Peer]] = {
    self.version.flatMap {
      case V24 | V23 | V22 | V21 | Unknown =>
        bitcoindCall[Vector[PeerPostV21]]("getpeerinfo")
      case V20 =>
        bitcoindCall[Vector[PeerV20]]("getpeerinfo")
      case V19 =>
        bitcoindCall[Vector[PeerPreV20]]("getpeerinfo")
    }
  }

  def listBanned: Future[Vector[NodeBan]] = {
    self.version.flatMap {
      case V22 | V23 | V24 | Unknown =>
        bitcoindCall[Vector[NodeBanPostV22]]("listbanned")
      case V21 | V20 =>
        bitcoindCall[Vector[NodeBanPostV20]]("listbanned")
      case V19 =>
        bitcoindCall[Vector[NodeBanPreV20]]("listbanned")
    }
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
