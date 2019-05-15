package org.bitcoins.testkit.node

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.AppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.networking.peer.{
  PeerHandler,
  PeerMessageReceiver,
  PeerMessageSender
}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, MustMatchers}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

trait NodeUnitTest
    extends BitcoinSFixture
    with MustMatchers
    with BitcoinSLogger
    with BeforeAndAfter
    with BeforeAndAfterAll {

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(appConfig)
  }

  override def afterAll(): Unit = {
    system.terminate()
    ()
  }

  implicit def system: ActorSystem

  implicit lazy val ec: ExecutionContext =
    system.dispatcher

  val timeout: FiniteDuration = 10.seconds

  implicit lazy val appConfig: AppConfig = NodeAppConfig

  implicit val np: NetworkParameters = appConfig.network

  lazy val startedBitcoindF = BitcoindRpcTestUtil.startedBitcoindRpcClient()

  lazy val bitcoindPeerF = startedBitcoindF.map(NodeTestUtil.getBitcoindPeer)

  def buildPeerMessageReceiver(): PeerMessageReceiver = {
    val receiver = PeerMessageReceiver.newReceiver(appConfig)
    receiver
  }

  def buildPeerHandler(): Future[PeerHandler] = {
    bitcoindPeerF.map { peer =>
      val peerMsgReceiver = buildPeerMessageReceiver()
      //the problem here is the 'self', this needs to be an ordinary peer message handler
      //that can handle the handshake
      val peerMsgSender: PeerMessageSender = {
        val client = NodeTestUtil.client(peer, peerMsgReceiver)
        PeerMessageSender(client, np)
      }

      PeerHandler(peerMsgReceiver, peerMsgSender)
    }

  }

  def peerSocketAddress(
      bitcoindRpcClient: BitcoindRpcClient): InetSocketAddress = {
    NodeTestUtil.getBitcoindSocketAddress(bitcoindRpcClient)
  }

}
