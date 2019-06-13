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
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.p2p.FilterLoadMessage
import org.bitcoins.core.p2p.NetworkPayload

case class SpvNode(
    peer: Peer,
    chainApi: ChainApi,
    bloomFilter: BloomFilter,
    callbacks: SpvNodeCallbacks = SpvNodeCallbacks.empty
)(
    implicit system: ActorSystem,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig)
    extends BitcoinSLogger {
  import system.dispatcher

  private val peerMsgRecv =
    PeerMessageReceiver.newReceiver(callbacks)

  private val client: Client =
    Client(context = system, peer = peer, peerMessageReceiver = peerMsgRecv)

  private val peerMsgSender: PeerMessageSender = {
    PeerMessageSender(client, nodeAppConfig.network)
  }

  /**
    * Sends the given P2P to our peer.
    * This method is useful for playing around
    * with P2P messages, therefore marked as
    * `private[node]`.
    */
  private[node] def send(msg: NetworkPayload): Unit = {
    peerMsgSender.sendMsg(msg)
  }

  /** Starts our spv node */
  def start(): Future[SpvNode] = {
    for {
      _ <- nodeAppConfig.initialize()
      node <- {
        peerMsgSender.connect()

        val isInitializedF =
          AsyncUtil.retryUntilSatisfied(peerMsgRecv.isInitialized)

        isInitializedF.failed.foreach(err =>
          logger.error(s"Failed to conenct with peer=$peer with err=${err}"))

        isInitializedF.map { _ =>
          logger.info(s"Our peer=${peer} has been initialized")
          this
        }
      }
    } yield {
      logger.info(s"Sending bloomfilter=${bloomFilter.hex} to $peer")
      val filterMsg = FilterLoadMessage(bloomFilter)
      val _ = send(filterMsg)
      node
    }
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
