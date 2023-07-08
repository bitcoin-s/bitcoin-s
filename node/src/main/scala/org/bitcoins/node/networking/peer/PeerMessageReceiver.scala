package org.bitcoins.node.networking.peer

import akka.actor.ActorSystem
import akka.stream.QueueOfferResult
import akka.stream.scaladsl.SourceQueueWithComplete
import org.bitcoins.core.api.node.{NodeType, Peer}
import org.bitcoins.core.p2p._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.networking.peer.PeerMessageReceiverState._
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
    peer: Peer,
    state: PeerMessageReceiverState
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
      s"Received message=${networkMsgRecv.msg.header.commandName} from peer=${peer} state=${state}")

    val payload = networkMsgRecv.msg.payload

    //todo: this works but doesn't seem to be the best place to do this
    val curState: PeerMessageReceiverState = {
      state match {
        case state: Waiting =>
          val responseFor = state.responseFor.asInstanceOf[ExpectsResponse]
          if (responseFor.isPayloadExpectedResponse(payload)) {
            val timeTaken = System.currentTimeMillis() - state.waitingSince
            logger.debug(
              s"Received expected response ${payload.commandName} in $timeTaken ms")
            state.expectedResponseCancellable.cancel()
            val newState = Normal(state.clientDisconnectP,
                                  state.versionMsgP,
                                  state.verackMsgP)
            newState
          } else state
        case state: Initializing =>
          if (payload == VerAckMessage)
            state.initializationTimeoutCancellable.cancel()
          state
        case _ => state
      }
    }

    networkMsgRecv.msg.payload match {
      case controlPayload: ControlPayload =>
        handleControlPayload(payload = controlPayload,
                             curReceiverState = curState)
          .map(newState => copy(state = newState))
      case dataPayload: DataPayload =>
        handleDataPayload(payload = dataPayload)
          .map(_ => copy(state = curState))
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
  private def handleControlPayload(
      payload: ControlPayload,
      curReceiverState: PeerMessageReceiverState): Future[
    PeerMessageReceiverState] = {
    controlMessageHandler
      .handleControlPayload(payload, peer, curReceiverState)
  }

  private def onResponseTimeout(
      networkPayload: NetworkPayload,
      peer: Peer): Future[PeerMessageReceiver] = {
    require(networkPayload.isInstanceOf[ExpectsResponse])
    logger.info(
      s"Handling response timeout for ${networkPayload.commandName} from $peer")

    //isn't this redundant? No, on response timeout may be called when not cancel timeout
    state match {
      case wait: Waiting => wait.expectedResponseCancellable.cancel()
      case _             =>
    }

    networkPayload match {
      case payload: ExpectsResponse =>
        logger.info(
          s"Response for ${payload.commandName} from $peer timed out in state $this")
        val qt = NodeStreamMessage.QueryTimeout(peer, payload)
        queue.offer(qt).map { _ =>
          state match {
            case _: Waiting if state.isInitialized =>
              val newState =
                Normal(state.clientDisconnectP,
                       state.versionMsgP,
                       state.verackMsgP)
              copy(state = newState)
            case _: PeerMessageReceiverState => this
          }
        }
      case _ =>
        logger.error(
          s"onResponseTimeout called for ${networkPayload.commandName} which does not expect response")
        Future.successful(this)
    }
  }

  def handleExpectResponse(msg: NetworkPayload, peer: Peer)(implicit
      system: ActorSystem,
      nodeAppConfig: NodeAppConfig): Future[PeerMessageReceiver] = {
    require(
      msg.isInstanceOf[ExpectsResponse],
      s"Cannot expect response for ${msg.commandName} from $peer as ${msg.commandName} does not expect a response.")
    import system.dispatcher
    state match {
      case good: Normal =>
        logger.debug(s"Handling expected response for ${msg.commandName}")
        val expectedResponseCancellable =
          system.scheduler.scheduleOnce(nodeAppConfig.queryWaitTime) {
            val offerF =
              queue.offer(
                NodeStreamMessage.SendResponseTimeout(peer = peer,
                                                      payload = msg))
            offerF.failed.foreach(err =>
              logger.error(
                s"Failed offering send response timeout waiting for response for peer=$peer",
                err))
          }
        val newState = Waiting(
          clientDisconnectP = good.clientDisconnectP,
          versionMsgP = good.versionMsgP,
          verackMsgP = good.verackMsgP,
          responseFor = msg,
          waitingSince = System.currentTimeMillis(),
          expectedResponseCancellable = expectedResponseCancellable
        )
        Future.successful(copy(state = newState))
      case state: Waiting =>
        logger.debug(
          s"Waiting for response to ${state.responseFor.commandName}. Ignoring next request for ${msg.commandName}")
        Future.successful(this)
      case bad @ (_: InitializedDisconnect | _: InitializedDisconnectDone |
          _: StoppedReconnect) =>
        throw new RuntimeException(
          s"Cannot expect response for ${msg.commandName} in state $bad")
      case Preconnection | _: Initializing | _: Disconnected =>
        //so we sent a message when things were good, but not we are back to connecting?
        //can happen when can happen where once we initialize the remote peer immediately disconnects us
        onResponseTimeout(msg, peer)
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
    state match {
      case bad @ (_: Initializing | _: Normal | _: InitializedDisconnect |
          _: InitializedDisconnectDone | _: Disconnected | _: StoppedReconnect |
          _: Waiting) =>
        throw new RuntimeException(s"Cannot call connect when in state=${bad}")
      case Preconnection =>
        logger.debug(s"Connection established with peer=${peer}")

        val initializationTimeoutCancellable =
          system.scheduler.scheduleOnce(nodeAppConfig.initializationTimeout) {
            val offerF =
              queue.offer(NodeStreamMessage.InitializationTimeout(peer))
            offerF.failed.foreach(err =>
              logger.error(s"Failed to offer initialize timeout for peer=$peer",
                           err))
          }

        val newState =
          Preconnection.toInitializing(initializationTimeoutCancellable)

        copy(state = newState)
    }
  }

  /** Initializes the disconnection from our peer on the network.
    * This is different than [[disconnect()]] as that indicates the
    * peer initialized a disconnection from us
    */
  private[networking] def initializeDisconnect(
      peer: Peer): PeerMessageReceiver = {
    logger.debug(s"Initializing disconnect from $peer")
    state match {
      case good @ (_: Disconnected) =>
        //if its already disconnected, just say init disconnect done so it wont reconnect
        logger.debug(s"Init disconnect called for already disconnected $peer")
        val newState = InitializedDisconnectDone(clientDisconnectP =
                                                   good.clientDisconnectP,
                                                 versionMsgP = good.versionMsgP,
                                                 verackMsgP = good.verackMsgP)
        copy(state = newState)
      case bad @ (_: InitializedDisconnectDone | Preconnection |
          _: StoppedReconnect) =>
        throw new RuntimeException(
          s"Cannot initialize disconnect from peer=$peer when in state=$bad")
      case _: InitializedDisconnect =>
        logger.warn(
          s"Already initialized disconnected from peer=$peer, this is a noop")
        this
      case initializing: Initializing =>
        initializing.initializationTimeoutCancellable.cancel()
        val newState = InitializedDisconnect(initializing.clientDisconnectP,
                                             initializing.versionMsgP,
                                             initializing.verackMsgP)
        copy(state = newState)
      case state: Normal =>
        val newState = InitializedDisconnect(state.clientDisconnectP,
                                             state.versionMsgP,
                                             state.verackMsgP)
        copy(state = newState)
      case state: Waiting =>
        val newState = InitializedDisconnect(state.clientDisconnectP,
                                             state.versionMsgP,
                                             state.verackMsgP)
        copy(state = newState)
    }
  }

  protected[networking] def disconnect(peer: Peer)(implicit
      system: ActorSystem): Future[PeerMessageReceiver] = {
    import system.dispatcher
    logger.debug(s"Disconnecting peer=$peer with internalstate=${this}")
    state match {
      case bad @ (_: Disconnected | Preconnection |
          _: InitializedDisconnectDone | _: StoppedReconnect) =>
        val exn = new RuntimeException(
          s"Cannot disconnect from peer=${peer} when in state=${bad}")
        Future.failed(exn)
      case good: InitializedDisconnect =>
        val newState = InitializedDisconnectDone(
          clientDisconnectP = good.clientDisconnectP.success(()),
          versionMsgP = good.versionMsgP,
          verackMsgP = good.verackMsgP)
        val disconnectedPeer = NodeStreamMessage.DisconnectedPeer(peer, false)
        queue.offer(disconnectedPeer).map(_ => copy(state = newState))
      case good @ (_: Normal | _: Waiting) =>
        logger.debug(s"Disconnected bitcoin peer=${peer}")
        val newState = Disconnected(
          clientDisconnectP = good.clientDisconnectP.success(()),
          versionMsgP = good.versionMsgP,
          verackMsgP = good.verackMsgP
        )
        val disconnectedPeer = NodeStreamMessage.DisconnectedPeer(peer, false)
        for {
          _ <- queue.offer(disconnectedPeer).map(_ => newState)
        } yield copy(state = newState)
      case initializing: Initializing =>
        initializing.initializationTimeoutCancellable.cancel()

        logger.debug(s"Disconnected bitcoin peer=${peer}")
        val newState = Disconnected(
          clientDisconnectP = initializing.clientDisconnectP.success(()),
          versionMsgP = initializing.versionMsgP,
          verackMsgP = initializing.verackMsgP
        )

        val disconnectedPeer = NodeStreamMessage.DisconnectedPeer(peer, false)
        for {
          _ <- queue.offer(disconnectedPeer)
        } yield copy(state = newState)
    }
  }
}

object PeerMessageReceiver {

  sealed abstract class PeerMessageReceiverMsg

  case class NetworkMessageReceived(msg: NetworkMessage, peer: Peer)
      extends PeerMessageReceiverMsg
}
