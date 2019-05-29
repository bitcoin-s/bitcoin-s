package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.Client
import org.bitcoins.node.networking.peer.{
  PeerMessageReceiver,
  PeerMessageSender
}
import org.bitcoins.rpc.util.AsyncUtil

import scala.concurrent.Future

case class SpvNode(peer: Peer, chainApi: ChainApi)(
    implicit system: ActorSystem,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig)
    extends BitcoinSLogger {
  import system.dispatcher

  private val peerMsgRecv =
    PeerMessageReceiver.newReceiver(nodeAppConfig, chainAppConfig)

  private val client: Client =
    Client(context = system, peer = peer, peerMessageReceiver = peerMsgRecv)

  private val peerMsgSender: PeerMessageSender = {
    PeerMessageSender(client, nodeAppConfig.network)
  }

  /** Starts our spv node */
  def start(): Future[SpvNode] = {
    peerMsgSender.connect()

    val isInitializedF =
      AsyncUtil.retryUntilSatisfied(peerMsgRecv.isInitialized)

    isInitializedF.map { _ =>
      logger.info(s"Our peer=${peer} has been initialized")
    }

    isInitializedF.failed.foreach { err =>
      logger.error(s"Failed to conenct with peer=$peer with err=${err}")

    }

    isInitializedF.map(_ => this)
  }

  /** Stops our spv node */
  def stop(): Future[SpvNode] = {
    peerMsgSender.disconnect()

    val isStoppedF = AsyncUtil.retryUntilSatisfied(peerMsgRecv.isDisconnected)

    isStoppedF.map(_ => this)
  }

  /** Checks if we have a tcp connection with our peer */
  def isConnected: Boolean = peerMsgRecv.isConnected

  /** Checks if we are fully initialized with our peer and have executed the handshake
    * This means we can now send arbitrary messages to our peer
    * @return
    */
  def isInitialized: Boolean = peerMsgRecv.isInitialized

  /** Starts to sync our spv node with our peer
    * If our local best block hash is the same as our peers
    * we will not sync, otherwise we will keep syncing
    * until our best block hashes match up
    * @return
    */
  def sync(): Future[Unit] = {
    chainApi.getBestBlockHash.map { hashBE: DoubleSha256DigestBE =>
      peerMsgSender.sendGetHeadersMessage(hashBE.flip)
    }
  }
}
