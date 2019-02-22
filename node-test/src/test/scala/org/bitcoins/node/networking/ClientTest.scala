package org.bitcoins.node.networking

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.io.Tcp
import akka.testkit.{TestActorRef, TestKit, TestProbe}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.db.UnitTestDbConfig
import org.bitcoins.node.networking.peer.PeerMessageReceiver
import org.bitcoins.node.util.NodeTestUtil
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.scalatest._

import scala.concurrent.Future

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
  val dbConfig = UnitTestDbConfig

  val bitcoindRpcF = BitcoindRpcTestUtil.startedBitcoindRpcClient()

  val bitcoindRemoteF = bitcoindRpcF.map { bitcoind =>
    NodeTestUtil.getBitcoindRemote(bitcoind)
  }

  val bitcoindRpc2F = BitcoindRpcTestUtil.startedBitcoindRpcClient()

  val bitcoindRemote2F = bitcoindRpcF.map { bitcoind =>
    NodeTestUtil.getBitcoindRemote(bitcoind)
  }

  behavior of "Client"

  it must "establish a tcp connection with a bitcoin node" in {
    val randomPort = BitcoindRpcTestUtil.randomPort
    bitcoindRemoteF.flatMap(remote => connectAndDisconnect(remote, randomPort))
  }

  it must "connect to two nodes" in {
    val randomPort = BitcoindRpcTestUtil.randomPort
    val try1 = bitcoindRemoteF.flatMap(remote =>
      connectAndDisconnect(remote, randomPort))

    val randomPort2 = BitcoindRpcTestUtil.randomPort
    val try2 = bitcoindRemote2F.flatMap(remote =>
      connectAndDisconnect(remote, randomPort2))

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
  def connectAndDisconnect(
      remote: InetSocketAddress,
      port: Int): Future[Assertion] = {
    val probe = TestProbe()

    val peerMessageReceiver = PeerMessageReceiver(dbConfig)
    val client = TestActorRef(Client.props(peerMessageReceiver), probe.ref)

    //random port
    client ! Tcp.Connect(remote, Some(new InetSocketAddress(port)))

    val isConnectedF =
      TestAsyncUtil.retryUntilSatisfied(peerMessageReceiver.isConnected)

    isConnectedF.flatMap { _ =>
      //disconnect here
      client ! Tcp.Abort
      val isDisconnectedF =
        TestAsyncUtil.retryUntilSatisfied(peerMessageReceiver.isDisconnected)

      isDisconnectedF.map { _ =>
        succeed
      }
    }
  }

  override def afterAll: Unit = {
    bitcoindRpcF.map(_.stop())
    TestKit.shutdownActorSystem(system)
  }

}
