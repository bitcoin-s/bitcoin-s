package org.bitcoins.node.networking

import akka.testkit.{TestActorRef, TestProbe}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient.ConnectCommand
import org.bitcoins.node.networking.peer.{
  PeerMessageReceiver,
  PeerMessageReceiverState
}
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.fixtures.BitcoinSAppConfigBitcoinFixtureStarted
import org.bitcoins.testkit.node.{NodeTestUtil, NodeUnitTest}
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.tor.CachedTor
import org.bitcoins.testkit.util.BitcoindRpcBaseTest
import org.scalatest.{Assertion, FutureOutcome}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class P2PClientActorTest
    extends BitcoinSAppConfigBitcoinFixtureStarted
    with BitcoindRpcBaseTest
    with CachedTor {

  override type FixtureParam = (BitcoinSAppConfig, BitcoinSAppConfig)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withTwoBitcoinSAppConfigNotStarted(test)

  lazy val bitcoindRpcF =
    BitcoindRpcTestUtil.startedBitcoindRpcClient(clientAccum = clientAccum)

  lazy val bitcoindPeerF = bitcoindRpcF.flatMap { bitcoind =>
    NodeTestUtil.getBitcoindPeer(bitcoind)
  }

  lazy val bitcoindRpc2F =
    BitcoindRpcTestUtil.startedBitcoindRpcClient(clientAccum = clientAccum)

  lazy val bitcoindPeer2F = bitcoindRpcF.flatMap { bitcoind =>
    NodeTestUtil.getBitcoindPeer(bitcoind)
  }

  lazy val probe: TestProbe = TestProbe()

  override def afterAll(): Unit = {
    super[BitcoindRpcBaseTest].afterAll()
    super[BitcoinSAppConfigBitcoinFixtureStarted].afterAll()
  }

  behavior of "P2PClientActorTest"

  it must "establish a tcp connection with a bitcoin node" in { tuple =>
    implicit val chainConf = tuple._1.chainConf
    implicit val nodeConf = tuple._1.nodeConf
    for {
      peer <- bitcoindPeerF
      client <- buildP2PClient(peer)
      res <- connectAndDisconnect(client)
    } yield res
  }

  it must "connect to two nodes" in { tuple =>
    val try1 = for {
      peer <- bitcoindPeerF
      client <- buildP2PClient(peer)(tuple._1.chainConf, tuple._1.nodeConf)
      res <- connectAndDisconnect(client)
    } yield res

    val try2 = for {
      peer <- bitcoindPeer2F
      client <- buildP2PClient(peer)(tuple._2.chainConf, tuple._2.nodeConf)
      res <- connectAndDisconnect(client)
    } yield res

    try1.flatMap { _ =>
      try2
    }
  }

  it must "close actor on disconnect" in { tuple =>
    for {
      peer <- bitcoindPeerF
      client <- buildP2PClient(peer)(tuple._1.chainConf, tuple._1.nodeConf)
      _ = probe.watch(client.actor)
      _ <- connectAndDisconnect(client)
      term = probe.expectTerminated(client.actor)
    } yield {
      assert(term.actor == client.actor)
    }
  }

  def buildP2PClient(peer: Peer)(implicit
      chainAppConfig: ChainAppConfig,
      nodeAppConfig: NodeAppConfig): Future[P2PClient] = {
    val peerMessageReceiverF =
      for {
        node <- NodeUnitTest.buildNode(peer, None)
      } yield PeerMessageReceiver(node, peer)

    val clientActorF: Future[TestActorRef[P2PClientActor]] =
      peerMessageReceiverF.map { peerMsgRecv =>
        TestActorRef(
          P2PClient.props(
            peer = peer,
            peerMsgHandlerReceiver = peerMsgRecv,
            peerMsgRecvState = PeerMessageReceiverState.fresh(),
            onReconnect = (_: Peer) => Future.unit,
            onStop = (_: Peer) => Future.unit,
            onInitializationTimeout = (_: Peer) => Future.unit,
            onQueryTimeout = (_, _) => Future.unit,
            sendResponseTimeout = (_, _) => Future.unit,
            maxReconnectionTries = 16
          ),
          probe.ref
        )
      }
    val p2pClientF: Future[P2PClient] = clientActorF.map {
      client: TestActorRef[P2PClientActor] =>
        P2PClient(client, peer)
    }
    p2pClientF
  }

  /** Helper method to connect to the
    * remote node and bind our local
    * connection to the specified port
    */
  private def connectAndDisconnect(p2pClient: P2PClient): Future[Assertion] = {
    p2pClient.actor ! ConnectCommand

    val isConnectedF = for {
      isConnected <- TestAsyncUtil.retryUntilSatisfiedF(p2pClient.isConnected,
                                                        1.second,
                                                        15)
    } yield isConnected

    isConnectedF.flatMap { _ =>
      p2pClient.actor ! P2PClient.CloseCommand
      val isDisconnectedF = for {
        isDisconnected <-
          TestAsyncUtil.retryUntilSatisfiedF(p2pClient.isDisconnected,
                                             interval = 1.second,
                                             maxTries = 100)
      } yield isDisconnected

      isDisconnectedF.map { _ =>
        succeed
      }
    }
  }
}
