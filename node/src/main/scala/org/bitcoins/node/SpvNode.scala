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
import org.bitcoins.node.messages.NetworkPayload
import org.bitcoins.node.models.InterestingPubKeyDAO
import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.bloom.BloomUpdateNone
import org.bitcoins.node.messages.control.FilterLoadMessage
import org.bitcoins.node.messages.FilterLoadMessage

/**
  * An SPV node that can send and receive P2P messages, construct
  * bloom filters based on an internal DB of elements of interest
  * and also provided callbacks for certain interesting events.
  *
  * @param bloomFilter The current bloom filter of the node, if any
  * @param callbacks Set of optional callbacks that get's executed
  *        at certain events in the node lifecycle.
  */
case class SpvNode(
    peer: Peer,
    chainApi: ChainApi,
    bloomFilter: Option[BloomFilter] = None,
    callbacks: SpvNodeCallbacks = SpvNodeCallbacks())(
    implicit system: ActorSystem,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig)
    extends BitcoinSLogger {
  import system.dispatcher

  private val pubkeyDAO = InterestingPubKeyDAO()

  /** Random UInt32 value used in constructing bloom filter */
  private val tweak: UInt32 = {
    val randomLong = math
      .floor(UInt32.max.toLong * math.random())
      // why does math.floor return a double???
      .toLong
    UInt32(randomLong)
  }

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

  /**
    * Adds the given public key to our table of
    * interesting public keys. This table is used
    * to construct a bloom filter we send to our
    * peer, which then will send us information
    * about transactions related to said keys.
    */
  def addPubKey(pub: ECPublicKey): Future[SpvNode] = {
    logger.debug(s"Marking $pub as an a public key of interest")
    for {
      _ <- pubkeyDAO.create(pub)
    } yield this
  }

  /** Starts our SPV node */
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

    isInitializedF.flatMap(_ => initBloomFilter())
  }

  /**
    * Constructs and sends a bloom filter based on
    * our table of pubkeys that's marked as interesting
    *
    * @return An SPV node with the newly set bloom filter
    */
  private def initBloomFilter(): Future[SpvNode] = {
    logger.info(s"Initializing bloom filter")
    for {
      pubs <- pubkeyDAO.findAll()
    } yield {
      if (pubs.isEmpty) {
        logger.info(
          s"No public keys registered with node, not sending bloom filter")
        this
      } else {
        val baseBloom =
          BloomFilter(numElements = pubs.length,
                      falsePositiveRate = nodeAppConfig.bloomFalsePositiveRate,
                      tweak = tweak,
                      // what's the best bloom update flag?
                      BloomUpdateNone)

        val bloomWithPubs = pubs.foldLeft(baseBloom)(_.insert(_))
        val createFiltermsg = FilterLoadMessage(bloomWithPubs)
        logger.info(
          s"Sending filterload with bloom filter for ${pubs.length} public key(s)")
        send(createFiltermsg)

        copy(bloomFilter = Some(bloomWithPubs))
      }
    }
  }

  /** Stops our SPV node */
  def stop(): Future[SpvNode] = {
    logger.info(s"Stopping SPV node")
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
