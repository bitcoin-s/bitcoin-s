package org.bitcoins.node.networking

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.PeerHandler
import org.bitcoins.rpc.config.{BitcoindConfig, BitcoindInstance}
import org.bitcoins.testkit.node.{
  CachedBitcoinSAppConfig,
  NodeTestUtil,
  NodeUnitTest
}
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import scala.concurrent.Future

class MaxConnectionsTest extends BitcoindRpcTest with CachedBitcoinSAppConfig {

  //we need a custom bitcoind config with maxconnections=0
  def writeNormalConfig: BitcoindConfig = BitcoindRpcTestUtil.standardConfig

  //need to start with maxconnections=0
  val maxConnections0Config =
    writeNormalConfig.withOption("maxconnections", "0")

  val instance = BitcoindInstance.fromConfig(maxConnections0Config)

  lazy val bitcoindRpcF =
    BitcoindRpcTestUtil.startedBitcoindRpcClient(instance = instance,
                                                 clientAccum = clientAccum)

  lazy val bitcoindPeerF: Future[Peer] =
    bitcoindRpcF.flatMap(b => NodeTestUtil.getBitcoindPeer(b))

  behavior of "MaxConnectionsTest"

  it must "attempt to reconnect if max connections are full" in {
    val peerHandlerF: Future[PeerHandler] = for {
      _ <- cachedConfig.start()
      peer <- bitcoindPeerF.flatMap(p => NodeUnitTest.buildPeerHandler(p))
    } yield peer

    val connectedF = for {
      peerHandler <- peerHandlerF
      _ = peerHandler.peerMsgSender.connect()
      _ <- AsyncUtil.retryUntilSatisfiedF(() =>
        peerHandler.p2pClient.isConnected())
    } yield succeed
    connectedF
  }
}
