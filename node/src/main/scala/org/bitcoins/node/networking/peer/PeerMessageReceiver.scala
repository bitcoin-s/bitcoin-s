package org.bitcoins.node.networking.peer

import akka.actor.ActorSystem
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.p2p._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.PeerMessageReceiverState._
import org.bitcoins.node.{Node, P2PLogger}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

/** Responsible for receiving messages from a peer on the
  * p2p network. This is called by [[org.bitcoins.rpc.client.common.Client Client]] when doing the p2p
  * handshake and during the [[PeerMessageReceiverState.Normal Normal]]
  * operations. This is the entry point for handling all received
  * [[org.bitcoins.core.p2p.NetworkMessage NetworkMessage]]
  */
class PeerMessageReceiver(
    node: Node,
    val state: PeerMessageReceiverState,
    peer: Peer
)(implicit system: ActorSystem, nodeAppConfig: NodeAppConfig)
    extends P2PLogger {
  import system.dispatcher

  require(nodeAppConfig.nodeType != NodeType.BitcoindBackend,
          "Bitcoind should handle the P2P interactions")

  /** This method is called when we have received
    * a [[akka.io.Tcp.Connected]] message from our peer
    * This means we have opened a Tcp connection,
    * but have NOT started the handshake
    * This method will initiate the handshake
    */
  protected[networking] def connect(client: P2PClient): PeerMessageReceiver = {

    state match {
      case bad @ (_: Initializing | _: Normal | _: InitializedDisconnect |
          _: InitializedDisconnectDone | _: Disconnected | _: StoppedReconnect |
          _: Waiting) =>
        throw new RuntimeException(s"Cannot call connect when in state=${bad}")
      case Preconnection =>
        logger.debug(s"Connection established with peer=${peer}")

        //todo: init timeout as param
        val timeout =
          system.scheduler.scheduleOnce(10.seconds)(onInitTimeout())

        val newState = Preconnection.toInitializing(client, timeout)

        val peerMsgSender = PeerMessageSender(client)

        peerMsgSender.sendVersionMessage(node.getDataMessageHandler.chainApi)

        val newRecv = toState(newState)

        newRecv
    }
  }

  /** Initializes the disconnection from our peer on the network.
    * This is different than [[disconnect()]] as that indicates the
    * peer initialized a disconnection from us
    */
  private[networking] def initializeDisconnect(): PeerMessageReceiver = {
    logger.debug(s"InitDisconnect for $peer")
    state match {
      //2022-06-21T04:52:58UTC WARN [P2PClientActor] Ignoring reconnection attempts as we initialized disconnect from peer=Peer(195.154.166.219:8333), state=InitializedDisconnect
      //2022-06-21T04:52:58UTC DEBUG [PeerManager] Client stopped for Peer(195.154.166.219:8333)
      //2022-06-21T04:52:58UTC DEBUG [PeerFinder] Removing peer Peer(195.154.166.219:8333)
      //2022-06-21T04:52:58UTC DEBUG [PeerMessageReceiver] Disconnected bitcoin peer=Peer(3.34.133.144:8333)
      //2022-06-21T04:52:58UTC INFO [P2PClientActor] Received close any state for Peer(3.34.133.144:8333)
      //2022-06-21T04:52:58UTC DEBUG [PeerMessageReceiver] InitDisconnect for Peer(3.34.133.144:8333)
      //2022-06-21T04:52:58UTC DEBUG [PeerManager] Client stopped for Peer(3.34.133.144:8333)
      //2022-06-21T04:52:58UTC DEBUG [PeerFinder] Removing peer Peer(3.34.133.144:8333)
      //case where we disconnected right before receiving an init disconnect request
      //in such a case the peer would try to reconnect again
      case good @ (_: Disconnected) =>
        //if its already disconnected, just say init disconnect done so it wont reconnect
        logger.debug(s"Init disconnect called for already disconnected $peer")
        val newState = InitializedDisconnectDone(
          clientConnectP = good.clientConnectP,
          clientDisconnectP = good.clientDisconnectP,
          versionMsgP = good.versionMsgP,
          verackMsgP = good.verackMsgP)
        new PeerMessageReceiver(node, newState, peer)
      case bad @ (_: InitializedDisconnectDone | Preconnection |
          _: StoppedReconnect) =>
        throw new RuntimeException(
          s"Cannot initialize disconnect from peer=$peer when in state=$bad")
      case _: InitializedDisconnect =>
        logger.warn(
          s"Already initialized disconnected from peer=$peer, this is a noop")
        this
      case state @ (_: Initializing | _: Normal) =>
        val newState = InitializedDisconnect(state.clientConnectP,
                                             state.clientDisconnectP,
                                             state.versionMsgP,
                                             state.verackMsgP)
        toState(newState)
      case state: Waiting =>
        onResponseTimeout(state.responseFor)
        val newState = InitializedDisconnect(state.clientConnectP,
                                             state.clientDisconnectP,
                                             state.versionMsgP,
                                             state.verackMsgP)
        toState(newState)
    }
  }

  def stopReconnect(): PeerMessageReceiver = {
    state match {
      case Preconnection =>
        //when retry, state should be back to preconnection
        val newState = StoppedReconnect(state.clientConnectP,
                                        state.clientDisconnectP,
                                        state.versionMsgP,
                                        state.verackMsgP)
        val newRecv = toState(newState)
        newRecv
      case _: StoppedReconnect =>
        logger.warn(
          s"Already stopping reconnect from peer=$peer, this is a noop")
        this
      case bad @ (_: Initializing | _: Normal | _: InitializedDisconnect |
          _: InitializedDisconnectDone | _: Disconnected | _: Waiting) =>
        throw new RuntimeException(
          s"Cannot stop reconnect from peer=$peer when in state=$bad")
    }
  }

  protected[networking] def disconnect(): PeerMessageReceiver = {
    logger.debug(s"Disconnecting with internalstate=${state}")
    state match {
      case bad @ (_: Disconnected | Preconnection |
          _: InitializedDisconnectDone | _: StoppedReconnect) =>
        throw new RuntimeException(
          s"Cannot disconnect from peer=${peer} when in state=${bad}")
      case good: InitializedDisconnect =>
        val newState = InitializedDisconnectDone(
          clientConnectP = good.clientConnectP,
          clientDisconnectP = good.clientDisconnectP.success(()),
          versionMsgP = good.versionMsgP,
          verackMsgP = good.verackMsgP)
        new PeerMessageReceiver(node, newState, peer)
      case good @ (_: Initializing | _: Normal | _: Waiting) =>
        good match {
          case wait: Waiting =>
            onResponseTimeout(wait.responseFor)
            wait.timeout.cancel()
          case wait: Initializing =>
            wait.timeout.cancel()
          //in this case we make rather try initializing again, as this was the existing behaviour
          //trying again would be done by reconnect
          case _ =>
        }

        logger.debug(s"Disconnected bitcoin peer=${peer}")
        val newState = Disconnected(
          clientConnectP = good.clientConnectP,
          clientDisconnectP = good.clientDisconnectP.success(()),
          versionMsgP = good.versionMsgP,
          verackMsgP = good.verackMsgP
        )

        new PeerMessageReceiver(node, newState, peer)
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

    val payload = networkMsgRecv.msg.payload

    //todo: this works but doesn't seem to be the best place to do this
    val curReceiver: PeerMessageReceiver = {
      state match {
        case state: Waiting =>
          val responseFor = state.responseFor.asInstanceOf[ExpectsResponse]
          if (responseFor.isPayloadExpectedResponse(payload)) {
            val timeTaken = System.currentTimeMillis() - state.waitingSince
            logger.debug(
              s"Received expected response ${payload.commandName} in $timeTaken ms")
            state.timeout.cancel()
            val newState = Normal(state.clientConnectP,
                                  state.clientDisconnectP,
                                  state.versionMsgP,
                                  state.verackMsgP)
            toState(newState)
          } else this
        case state: Initializing =>
          if (payload == VerAckMessage)
            state.timeout.cancel()
          this
        case _ => this
      }
    }

    networkMsgRecv.msg.payload match {
      case controlPayload: ControlPayload =>
        handleControlPayload(payload = controlPayload,
                             sender = peerMsgSender,
                             curReceiver)
      case dataPayload: DataPayload =>
        handleDataPayload(payload = dataPayload,
                          sender = peerMsgSender,
                          curReceiver)
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
      payload: DataPayload,
      sender: PeerMessageSender,
      curReceiver: PeerMessageReceiver): Future[PeerMessageReceiver] = {
    //else it means we are receiving this data payload from a peer,
    //we need to handle it
    node.getDataMessageHandler.handleDataPayload(payload, sender, peer).map {
      handler =>
        val newNode = node.updateDataMessageHandler(handler)
        new PeerMessageReceiver(newNode, curReceiver.state, peer)
    }
  }

  /** Handles control payloads defined here https://bitcoin.org/en/developer-reference#control-messages
    *
    * @param payload the payload we need to do something with
    * @param sender the [[PeerMessageSender]] we can use to initialize an subsequent messages that need to be sent
    * @return the requests with the request removed for which the @payload is responding too
    */
  private def handleControlPayload(
      payload: ControlPayload,
      sender: PeerMessageSender,
      curReceiver: PeerMessageReceiver): Future[PeerMessageReceiver] = {
    node.controlMessageHandler
      .handleControlPayload(payload, sender, peer, curReceiver)
  }

  def onInitTimeout(): Unit = {
    logger.debug(s"Init timeout for peer $peer")
    node.peerManager.onInitializationTimeout(peer)
  }

  def onResponseTimeout(networkPayload: NetworkPayload): Unit = {
    assert(networkPayload.isInstanceOf[ExpectsResponse])
    logger.debug(
      s"Called on Response Timeout for ${networkPayload.commandName}")

    //isn't this redundant? No, on response timeout may be called when not cancel timeout
    state match {
      case wait: Waiting => wait.timeout.cancel()
      case _             =>
    }

    networkPayload match {
      case payload: ExpectsResponse =>
        logger.info(
          s"Response for ${payload.commandName} from $peer timed out.")
        node.peerManager.onQueryTimeout(payload, peer).foreach(_ => ())
      case _ =>
        logger.error(
          s"onResponseTimeout called for ${networkPayload.commandName} which does not expect response")
    }
  }

  def handleExpectResponse(msg: NetworkPayload): PeerMessageReceiver = {
    state match {
      case good: Normal =>
        logger.debug(s"handling expected response for ${msg.commandName}")
        val timeout =
          system.scheduler.scheduleOnce(15.seconds)(onResponseTimeout(msg))
        val newState = Waiting(
          clientConnectP = good.clientConnectP,
          clientDisconnectP = good.clientDisconnectP,
          versionMsgP = good.versionMsgP,
          verackMsgP = good.verackMsgP,
          responseFor = msg,
          waitingSince = System.currentTimeMillis(),
          timeout = timeout
        )
        logger.debug(s"handleExpectResponse: Current state is now $newState")
        toState(newState)
      case state: Waiting =>
        logger.debug(
          s"Waiting for response to ${state.responseFor.commandName}. Ignoring next request for ${msg.commandName}")
        this
      case bad @ (_: InitializedDisconnect | _: InitializedDisconnectDone |
          _: StoppedReconnect) =>
        throw new RuntimeException(
          s"Cannot expect response for ${msg.commandName} in state $bad")
      case Preconnection | _: Initializing | _: Disconnected =>
        //so we sent a message when things were good, but not we are back to connecting?
        //can happen when can happen where once we intiailize the remote peer immediately disconnects us
        //note that we do handle a disconnect when we are in state Waiting and are disconnected upon
        //but the case where we are disconnected before we could send a message i.e. we were in Normal state
        //so now the expect request should immediately fail
        onResponseTimeout(msg)
        this

    }
  }

  /** Transitions our PeerMessageReceiver to a new state */
  def toState(newState: PeerMessageReceiverState): PeerMessageReceiver = {
    new PeerMessageReceiver(
      node = node,
      state = newState,
      peer = peer
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

  def apply(state: PeerMessageReceiverState, node: Node, peer: Peer)(implicit
      system: ActorSystem,
      nodeAppConfig: NodeAppConfig
  ): PeerMessageReceiver = {
    new PeerMessageReceiver(node = node, state = state, peer = peer)
  }

  /** Creates a peer message receiver that is ready
    * to be connected to a peer. This can be given to [[org.bitcoins.node.networking.P2PClient.props() P2PClient]]
    * to connect to a peer on the network
    */
  def preConnection(peer: Peer, node: Node)(implicit
      system: ActorSystem,
      nodeAppConfig: NodeAppConfig
  ): PeerMessageReceiver = {
    PeerMessageReceiver(node = node,
                        state = PeerMessageReceiverState.fresh(),
                        peer = peer)
  }

  def newReceiver(node: Node, peer: Peer)(implicit
      nodeAppConfig: NodeAppConfig,
      system: ActorSystem): PeerMessageReceiver = {
    PeerMessageReceiver(state = PeerMessageReceiverState.fresh(),
                        node = node,
                        peer = peer)
  }
}
