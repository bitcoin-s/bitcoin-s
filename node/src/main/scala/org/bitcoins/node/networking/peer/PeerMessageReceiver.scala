package org.bitcoins.node.networking.peer

import akka.actor.ActorSystem
import akka.stream.QueueOfferResult
import akka.stream.scaladsl.SourceQueueWithComplete
import org.bitcoins.core.api.node.{NodeType, Peer}
import org.bitcoins.core.p2p._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.{NodeStreamMessage, P2PLogger}

import scala.concurrent.Future

/** Responsible for receiving messages from a peer on the
  * p2p network. This is called by [[org.bitcoins.rpc.client.common.Client Client]] when doing the p2p
  * handshake and during the [[PeerMessageReceiverState.Normal Normal]]
  * operations. This is the entry point for handling all received
  * [[org.bitcoins.core.p2p.NetworkMessage NetworkMessage]]
  */
case class PeerMessageReceiver(
    controlMessageHandler: ControlMessageHandler,
    queue: SourceQueueWithComplete[NodeStreamMessage],
    peer: Peer
)(implicit system: ActorSystem, nodeAppConfig: NodeAppConfig)
    extends P2PLogger {
  import system.dispatcher

  require(nodeAppConfig.nodeType != NodeType.BitcoindBackend,
          "Bitcoind should handle the P2P interactions")

  def handleNetworkMessageReceived(
      networkMsgRecv: PeerMessageReceiver.NetworkMessageReceived): Future[
    PeerMessageReceiver] = {

    val peer = networkMsgRecv.peer

    logger.debug(
      s"Received message=${networkMsgRecv.msg.header.commandName} from peer=${peer}")

    networkMsgRecv.msg.payload match {
      case controlPayload: ControlPayload =>
        handleControlPayload(payload = controlPayload)
          .map(_ => this)
      case dataPayload: DataPayload =>
        handleDataPayload(payload = dataPayload)
          .map(_ => this)
    }
  }

  /** Handles a [[DataPayload]] message. It checks if the sender is the parent
    * actor, it sends it to our peer on the network. If the sender was the
    * peer on the network, forward to the actor that spawned our actor
    *
    * @param payload
    * @param sender
    */
  private def handleDataPayload(
      payload: DataPayload): Future[QueueOfferResult] = {
    //else it means we are receiving this data payload from a peer,
    //we need to handle it
    val wrapper = NodeStreamMessage.DataMessageWrapper(payload, peer)

    queue.offer(wrapper)
  }

  /** Handles control payloads defined here https://bitcoin.org/en/developer-reference#control-messages
    *
    * @param payload the payload we need to do something with
    * @param sender the [[PeerMessageSender]] we can use to initialize an subsequent messages that need to be sent
    * @return the requests with the request removed for which the @payload is responding too
    */
  private def handleControlPayload(payload: ControlPayload): Future[Unit] = {
    controlMessageHandler
      .handleControlPayload(payload, peer)
  }

  private def onResponseTimeout(
      networkPayload: NetworkPayload,
      peer: Peer): Future[PeerMessageReceiver] = {
    require(networkPayload.isInstanceOf[ExpectsResponse])
    logger.info(
      s"Handling response timeout for ${networkPayload.commandName} from $peer")

    networkPayload match {
      case payload: ExpectsResponse =>
        logger.info(
          s"Response for ${payload.commandName} from $peer timed out in state $this")
        val qt = NodeStreamMessage.QueryTimeout(peer, payload)
        queue.offer(qt).map(_ => this)
      case _ =>
        logger.error(
          s"onResponseTimeout called for ${networkPayload.commandName} which does not expect response")
        Future.successful(this)
    }
  }

  /** This method is called when we have received
    * a [[akka.io.Tcp.Connected]] message from our peer
    * This means we have opened a Tcp connection,
    * but have NOT started the handshake
    * This method will initiate the handshake
    */
  protected[networking] def connect(peer: Peer)(implicit
      system: ActorSystem,
      nodeAppConfig: NodeAppConfig): PeerMessageReceiver = {
    import system.dispatcher
    val initializationTimeoutCancellable =
      system.scheduler.scheduleOnce(nodeAppConfig.initializationTimeout) {
        val offerF =
          queue.offer(NodeStreamMessage.InitializationTimeout(peer))
        offerF.failed.foreach(err =>
          logger.error(s"Failed to offer initialize timeout for peer=$peer",
                       err))
      }

    this
  }

  protected[networking] def disconnect(peer: Peer)(implicit
      system: ActorSystem): Future[PeerMessageReceiver] = {
    import system.dispatcher
    logger.debug(s"Disconnecting peer=$peer with internalstate=${this}")
    val disconnectedPeer = NodeStreamMessage.DisconnectedPeer(peer, false)
    for {
      _ <- queue.offer(disconnectedPeer)
    } yield this
  }
}

object PeerMessageReceiver {

  sealed abstract class PeerMessageReceiverMsg

  case class NetworkMessageReceived(msg: NetworkMessage, peer: Peer)
      extends PeerMessageReceiverMsg
}
