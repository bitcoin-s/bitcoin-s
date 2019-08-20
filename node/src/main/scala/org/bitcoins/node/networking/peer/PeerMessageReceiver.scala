package org.bitcoins.node.networking.peer

import akka.actor.ActorRefFactory
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.p2p.{NetworkMessage, _}
import org.bitcoins.node.P2PLogger
import org.bitcoins.node.SpvNodeCallbacks
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.PeerMessageReceiverState.{
  Disconnected,
  Initializing,
  Normal,
  Preconnection
}

import scala.concurrent.Future

/**
  * Responsible for receiving messages from a peer on the
  * p2p network. This is called by [[org.bitcoins.rpc.client.common.Client Client]] when doing the p2p
  * handshake and during the [[PeerMessageReceiverState.Normal Normal]]
  * operations. This is the entry point for handling all received
  * [[org.bitcoins.core.p2p.NetworkMessage NetworkMessage]]
  */
class PeerMessageReceiver(
    dataMessageHandler: DataMessageHandler,
    val state: PeerMessageReceiverState,
    peer: Peer,
    callbacks: SpvNodeCallbacks
)(
    implicit ref: ActorRefFactory,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig)
    extends P2PLogger {
  import ref.dispatcher

  /** This method is called when we have received
    * a [[akka.io.Tcp.Connected]] message from our peer
    * This means we have opened a Tcp connection,
    * but have NOT started the handshake
    * This method will initiate the handshake
    */
  protected[networking] def connect(
      client: P2PClient): Future[PeerMessageReceiver] = {

    state match {
      case bad @ (_: Initializing | _: Normal | _: Disconnected) =>
        Future.failed(
          new RuntimeException(s"Cannot call connect when in state=${bad}")
        )
      case Preconnection =>
        logger.info(s"Connection established with peer=${peer}")

        val newState = Preconnection.toInitializing(client)

        val peerMsgSender = PeerMessageSender(client)

        peerMsgSender.sendVersionMessage()

        val newRecv = toState(newState)

        Future.successful(newRecv)
    }
  }

  protected[networking] def disconnect(): Future[PeerMessageReceiver] = {
    logger.trace(s"Disconnecting with internalstate=${state}")
    state match {
      case bad @ (_: Initializing | _: Disconnected | Preconnection) =>
        Future.failed(
          new RuntimeException(
            s"Cannot disconnect from peer=${peer} when in state=${bad}")
        )

      case good: Normal =>
        logger.debug(s"Disconnected bitcoin peer=${peer}")
        val newState = Disconnected(
          clientConnectP = good.clientConnectP,
          clientDisconnectP = good.clientDisconnectP.success(()),
          versionMsgP = good.versionMsgP,
          verackMsgP = good.verackMsgP
        )

        val newRecv = toState(newState)

        Future.successful(newRecv)
    }
  }

  private[networking] def isConnected: Boolean = state.isConnected

  private[networking] def isDisconnected: Boolean = state.isDisconnected

  private[networking] def hasReceivedVersionMsg: Boolean =
    state.hasReceivedVersionMsg.isCompleted

  private[networking] def hasReceivedVerackMsg: Boolean =
    state.hasReceivedVerackMsg.isCompleted

  private[networking] def isInitialized: Boolean = state.isInitialized

  def handleNetworkMessageReceived(
      networkMsgRecv: PeerMessageReceiver.NetworkMessageReceived): Future[
    PeerMessageReceiver] = {

    val client = networkMsgRecv.client

    //create a way to send a response if we need too
    val peerMsgSender = PeerMessageSender(client)

    logger.debug(
      s"Received message=${networkMsgRecv.msg.header.commandName} from peer=${client.peer} state=${state} ")
    networkMsgRecv.msg.payload match {
      case controlPayload: ControlPayload =>
        val peerMsgRecvF =
          handleControlPayload(payload = controlPayload, sender = peerMsgSender)
        peerMsgRecvF
      case dataPayload: DataPayload =>
        handleDataPayload(payload = dataPayload, sender = peerMsgSender)
    }
  }

  /**
    * Handles a [[DataPayload]] message. It checks if the sender is the parent
    * actor, it sends it to our peer on the network. If the sender was the
    * peer on the network, forward to the actor that spawned our actor
    *
    * @param payload
    * @param sender
    */
  private def handleDataPayload(
      payload: DataPayload,
      sender: PeerMessageSender): Future[PeerMessageReceiver] = {
    //else it means we are receiving this data payload from a peer,
    //we need to handle it
    val newDataMessageHandlerF =
      dataMessageHandler.handleDataPayload(payload, sender)

    newDataMessageHandlerF.map { handler =>
      new PeerMessageReceiver(handler, state, peer, callbacks)
    }
  }

  /**
    * Handles control payloads defined here https://bitcoin.org/en/developer-reference#control-messages
    *
    * @param payload  the payload we need to do something with
    * @param sender the [[PeerMessageSender]] we can use to initialize an subsequent messages that need to be sent
    * @return the requests with the request removed for which the @payload is responding too
    */
  private def handleControlPayload(
      payload: ControlPayload,
      sender: PeerMessageSender): Future[PeerMessageReceiver] = {
    payload match {

      case versionMsg: VersionMessage =>
        logger.trace(s"Received versionMsg=${versionMsg}from peer=${peer}")

        state match {
          case bad @ (_: Disconnected | _: Normal | Preconnection) =>
            Future.failed(
              new RuntimeException(
                s"Cannot handle version message while in state=${bad}"))

          case good: Initializing =>
            val newState = good.withVersionMsg(versionMsg)

            sender.sendVerackMessage()

            val newRecv = toState(newState)

            Future.successful(newRecv)
        }

      case VerAckMessage =>
        state match {
          case bad @ (_: Disconnected | _: Normal | Preconnection) =>
            Future.failed(
              new RuntimeException(
                s"Cannot handle version message while in state=${bad}"))

          case good: Initializing =>
            val newState = good.toNormal(VerAckMessage)
            val newRecv = toState(newState)
            Future.successful(newRecv)
        }

      case ping: PingMessage =>
        sender.sendPong(ping)
        Future.successful(this)
      case SendHeadersMessage =>
        //we want peers to just send us headers
        //we don't want to have to request them manually
        sender.sendHeadersMessage()
        Future.successful(this)
      case _: AddrMessage =>
        Future.successful(this)
      case _ @(_: FilterAddMessage | _: FilterLoadMessage |
          FilterClearMessage) =>
        Future.successful(this)
      case _ @(GetAddrMessage | _: PongMessage) =>
        Future.successful(this)
      case _: RejectMessage =>
        Future.successful(this)
      case _: FeeFilterMessage =>
        Future.successful(this)
    }
  }

  /** Transitions our PeerMessageReceiver to a new state */
  def toState(newState: PeerMessageReceiverState): PeerMessageReceiver = {
    new PeerMessageReceiver(
      dataMessageHandler = dataMessageHandler,
      state = newState,
      peer = peer,
      callbacks = callbacks
    )
  }
}

object PeerMessageReceiver {

  sealed abstract class PeerMessageReceiverMsg {

    /** Who we need to use to send a reply to our peer
      * if a response is needed for this message
      */
    def client: P2PClient
  }

  case class NetworkMessageReceived(msg: NetworkMessage, client: P2PClient)
      extends PeerMessageReceiverMsg

  def apply(
      state: PeerMessageReceiverState,
      chainApi: ChainApi,
      peer: Peer,
      callbacks: SpvNodeCallbacks)(
      implicit ref: ActorRefFactory,
      nodeAppConfig: NodeAppConfig,
      chainAppConfig: ChainAppConfig
  ): PeerMessageReceiver = {
    import ref.dispatcher
    val dataHandler = new DataMessageHandler(chainApi, callbacks)
    new PeerMessageReceiver(dataMessageHandler = dataHandler,
                            state = state,
                            peer = peer,
                            callbacks = callbacks)
  }

  /**
    * Creates a peer message receiver that is ready
    * to be connected to a peer. This can be given to [[org.bitcoins.node.networking.P2PClient.props() P2PClient]]
    * to connect to a peer on the network
    */
  def preConnection(peer: Peer, callbacks: SpvNodeCallbacks)(
      implicit ref: ActorRefFactory,
      nodeAppConfig: NodeAppConfig,
      chainAppConfig: ChainAppConfig
  ): Future[PeerMessageReceiver] = {
    import ref.dispatcher
    val blockHeaderDAO = BlockHeaderDAO()
    val chainHandlerF =
      ChainHandler.fromDatabase(blockHeaderDAO)
    for {
      chainHandler <- chainHandlerF
    } yield {
      PeerMessageReceiver(state = PeerMessageReceiverState.fresh(),
                          chainApi = chainHandler,
                          peer = peer,
                          callbacks = callbacks)
    }
  }

  def newReceiver(chainApi: ChainApi, peer: Peer, callbacks: SpvNodeCallbacks)(
      implicit nodeAppConfig: NodeAppConfig,
      chainAppConfig: ChainAppConfig,
      ref: ActorRefFactory): PeerMessageReceiver = {
    PeerMessageReceiver(state = PeerMessageReceiverState.fresh(),
                        chainApi = chainApi,
                        peer = peer,
                        callbacks = callbacks)
  }
}
