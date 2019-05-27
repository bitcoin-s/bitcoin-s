package org.bitcoins.node.networking

import akka.actor.ActorSystem
import akka.io.Tcp
import akka.testkit.{TestActorRef, TestKit, TestProbe}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.PeerMessageReceiver
import org.bitcoins.node.networking.peer.PeerMessageReceiverState.Preconnection
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.node.NodeTestUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.scalatest._

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

/**
  * Created by chris on 6/7/16.
  */
class ClientTest
    extends AsyncFlatSpec
    with MustMatchers
    with BeforeAndAfter
    with BeforeAndAfterAll
    with BitcoinSLogger {
  implicit val system = ActorSystem(
    s"Client-Test-System-${System.currentTimeMillis()}")

  private val appConfig = NodeTestUtil.nodeAppConfig

  private val chainAppConfig = ChainAppConfig()

  implicit val np = appConfig.network

  val bitcoindRpcF = BitcoindRpcTestUtil.startedBitcoindRpcClient()

  val bitcoindPeerF = bitcoindRpcF.map { bitcoind =>
    NodeTestUtil.getBitcoindPeer(bitcoind)
  }

  val bitcoindRpc2F = BitcoindRpcTestUtil.startedBitcoindRpcClient()

  val bitcoindPeer2F = bitcoindRpcF.map { bitcoind =>
    NodeTestUtil.getBitcoindPeer(bitcoind)
  }

  behavior of "Client"

  it must "establish a tcp connection with a bitcoin node" in {
    bitcoindPeerF.flatMap(remote => connectAndDisconnect(remote))
  }

  it must "connect to two nodes" in {
    val try1 =
      bitcoindPeerF.flatMap(remote => connectAndDisconnect(remote))

    val try2 = bitcoindPeer2F.flatMap(remote => connectAndDisconnect(remote))

    try1.flatMap { _ =>
      try2
    }
  }

  /**
    * Helper method to connect to the
    * remote node and bind our local
    * connection to the specified port
    * @param remote the remote node on the p2p network we are connecting to
    * @param port the port we are binding on our machine
    * @return
    */
  def connectAndDisconnect(peer: Peer): Future[Assertion] = {
    val probe = TestProbe()
    val remote = peer.socket
    val peerMessageReceiver =
      PeerMessageReceiver(state = Preconnection,
                          nodeAppConfig = appConfig,
                          chainAppConfig = chainAppConfig)
    val client =
      TestActorRef(Client.props(peer, peerMessageReceiver), probe.ref)

    client ! Tcp.Connect(remote)

    val isConnectedF =
      TestAsyncUtil.retryUntilSatisfied(peerMessageReceiver.isInitialized)

    isConnectedF.flatMap { _ =>
      //disconnect here
      client ! Tcp.Abort
      val isDisconnectedF =
        TestAsyncUtil.retryUntilSatisfied(peerMessageReceiver.isDisconnected,
                                          duration = 1.seconds)

      isDisconnectedF.map { _ =>
        succeed
      }
    }
  }

  override def afterAll: Unit = {
    bitcoindRpcF.flatMap(_.stop())
    bitcoindRpc2F.flatMap(_.stop())
    TestKit.shutdownActorSystem(system)
  }

}
