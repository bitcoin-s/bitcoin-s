package org.bitcoins.node.networking

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.asyncutil.AsyncUtil.RpcRetryException
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.PeerHandler
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.testkit.node.{
  CachedBitcoinSAppConfig,
  NodeTestUtil,
  NodeUnitTest
}
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.{AkkaUtil, BitcoindRpcTest}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class MaxConnectionsTest extends BitcoindRpcTest with CachedBitcoinSAppConfig {

  val standardConfig = BitcoindRpcTestUtil.standardConfig

  val datadir = standardConfig.datadir

  //need to start with maxconnections=0
  val maxConnections0Config =
    standardConfig.withOption("maxconnections", "0")

  val binary = BitcoindRpcTestUtil.getBinary(BitcoindVersion.newest)

  val instance =
    BitcoindInstance.fromConfig(config = maxConnections0Config, binary = binary)

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
      bitcoindRpc <- bitcoindRpcF

      _ = peerHandler.peerMsgSender.connect()
      _ <- AsyncUtil
        .retryUntilSatisfiedF(() => peerHandler.p2pClient.isConnected())
        .recover { case _: RpcRetryException =>
          //expect this to fail, we cannot connect
          //because maxconnections=0
          ()
        }
      //we need to wait for the re-connection attempts
      //to at least a 16 second delay before reconnecting
      //so we have time to restart bitcoind
      _ = AkkaUtil.nonBlockingSleep(10.seconds)
      _ <- bitcoindRpc.stop()
      //write updated configuration, this sets maxconnections=1
      //which allows us to connect
      _ = standardConfig.withOption("maxconnections", "125")
      _ <- bitcoindRpc.start()
      _ = logger.error(s"Done starting bitcoind again")
      //now we should eventually automatically reconnect
      _ <- AsyncUtil.retryUntilSatisfiedF(
        conditionF = () => peerHandler.p2pClient.isConnected(),
        interval = 500.millis,
        maxTries = 60)
    } yield succeed

    connectedF
  }
}
