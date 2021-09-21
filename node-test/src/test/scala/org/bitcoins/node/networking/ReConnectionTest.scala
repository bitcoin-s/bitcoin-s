package org.bitcoins.node.networking

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.asyncutil.AsyncUtil.RpcRetryException
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.PeerHandler
import org.bitcoins.testkit.node.{NodeTestUtil, NodeUnitTest}
import org.bitcoins.testkit.rpc.{BitcoindRpcTestUtil, BitcoindRpcTorTest}
import org.bitcoins.testkit.util.AkkaUtil

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class ReConnectionTest extends BitcoindRpcTorTest {

  lazy val bitcoindRpcF =
    BitcoindRpcTestUtil.startedBitcoindRpcClient(torAppConfigOpt = torConfigOpt,
                                                 clientAccum = clientAccum)

  lazy val bitcoindPeerF: Future[Peer] =
    bitcoindRpcF.flatMap(b =>
      NodeTestUtil.getBitcoindPeer(b,
                                   torConfigOpt.flatMap(_.socks5ProxyParams)))

  behavior of "ReConnectionTest"

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
      _ <- bitcoindRpc.stop()
      //need to wait for mac to unlock the datadir
      //before we can restart the bitcoind binary
      _ <- AkkaUtil.nonBlockingSleep(3.seconds)
      _ <- bitcoindRpc.start()
      //now we should eventually automatically reconnect
      _ <- AsyncUtil.retryUntilSatisfiedF(
        conditionF = () => peerHandler.p2pClient.isConnected(),
        interval = 500.millis,
        maxTries = 60)
    } yield succeed

    connectedF
  }
}
