package org.bitcoins.node.networking.peer

import akka.actor.{ActorSystem, Cancellable}
import grizzled.slf4j.Logging
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.p2p.{
  ExpectsResponse,
  NetworkPayload,
  VerAckMessage,
  VersionMessage
}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.{P2PClient, P2PClientCallbacks}
import org.bitcoins.node.networking.peer.PeerMessageReceiverState.{
  Disconnected,
  InitializedDisconnect,
  InitializedDisconnectDone,
  Initializing,
  Normal,
  Preconnection,
  StoppedReconnect,
  Waiting
}

import scala.concurrent.{ExecutionContext, Future, Promise}

sealed abstract class PeerMessageReceiverState extends Logging {

  /** This promise gets completed when we receive a
    * [[akka.io.Tcp.Connected]] message from [[org.bitcoins.node.networking.P2PClient P2PClient]]
    */
  def clientConnectP: Promise[P2PClient]

  /** The [[org.bitcoins.node.networking.P2PClient P2PClient]] we are
    * connected to. This isn't initiated until the client
    * has called [[org.bitcoins.node.networking.peer.PeerMessageReceiver.connect() connect()]]
    */
  private val clientConnectF: Future[P2PClient] = clientConnectP.future

  /** This promise is completed in the [[org.bitcoins.node.networking.peer.PeerMessageReceiver.disconnect() disconnect()]]
    * when a [[org.bitcoins.node.networking.P2PClient P2PClient]] initiates a disconnections from
    * our peer on the p2p network
    */
  def clientDisconnectP: Promise[Unit]

  private val clientDisconnectF: Future[Unit] = clientDisconnectP.future

  /** If this future is completed, we are
    * connected to our client. Note, there is
    * no timeout on this future and no guarantee
    * that some one has actually initiated
    * a connection with a [[org.bitcoins.node.networking.P2PClient P2PClient]]
    * @return
    */
  def isConnected: Boolean = {
    clientConnectF.isCompleted && !clientDisconnectF.isCompleted
  }

  def isDisconnected: Boolean = {
    clientDisconnectF.isCompleted && !isConnected
  }

  def versionMsgP: Promise[VersionMessage]

  /** This future is completed when our peer has sent
    * us their [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] indicating what protocol
    * features they support
    */
  def hasReceivedVersionMsg: Future[VersionMessage] = {
    versionMsgP.future
  }

  def verackMsgP: Promise[VerAckMessage.type]

  /** This future completes when we have received a
    * [[org.bitcoins.core.p2p.VerAckMessage VerAckMessage]] from our peer. This means our
    * peer has accepted our [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] and is
    * willing to connect with us
    * @return
    */
  def hasReceivedVerackMsg: Future[VerAckMessage.type] = {
    verackMsgP.future
  }

  /** Indicates we have connected and completed the initial
    * handshake that is required to connect to the bitcoin p2p network
    * If this is true, we can start sending and receiving normal
    * [[org.bitcoins.core.p2p.NetworkMessage NetworkMessage]] with our peer on the network
    * @return
    */
  def isInitialized: Boolean = {
    hasReceivedVersionMsg.isCompleted && hasReceivedVerackMsg.isCompleted
  }

  /** This method is called when we have received
    * a [[akka.io.Tcp.Connected]] message from our peer
    * This means we have opened a Tcp connection,
    * but have NOT started the handshake
    * This method will initiate the handshake
    */
  protected[networking] def connect(
      client: P2PClient,
      onInitializationTimeout: Peer => Future[Unit])(implicit
      system: ActorSystem,
      nodeAppConfig: NodeAppConfig,
      chainAppConfig: ChainAppConfig): PeerMessageReceiverState.Initializing = {
    import system.dispatcher
    val peer = client.peer
    this match {
      case bad @ (_: Initializing | _: Normal | _: InitializedDisconnect |
          _: InitializedDisconnectDone | _: Disconnected | _: StoppedReconnect |
          _: Waiting) =>
        throw new RuntimeException(s"Cannot call connect when in state=${bad}")
      case Preconnection =>
        logger.debug(s"Connection established with peer=${client.peer}")

        val initializationTimeoutCancellable =
          system.scheduler.scheduleOnce(nodeAppConfig.initializationTimeout) {
            val timeoutF = onInitializationTimeout(peer)
            timeoutF.failed.foreach(err =>
              logger.error(s"Failed to initialize timeout for peer=$peer", err))
          }

        val newState =
          Preconnection.toInitializing(client, initializationTimeoutCancellable)

        val peerMsgSender = PeerMessageSender(client)
        val chainApi = ChainHandler.fromDatabase()
        peerMsgSender.sendVersionMessage(chainApi)

        newState
    }
  }

  /** Initializes the disconnection from our peer on the network.
    * This is different than [[disconnect()]] as that indicates the
    * peer initialized a disconnection from us
    */
  private[networking] def initializeDisconnect(
      peer: Peer): PeerMessageReceiverState = {
    logger.debug(s"Initializing disconnect from $peer")
    this match {
      case good @ (_: Disconnected) =>
        //if its already disconnected, just say init disconnect done so it wont reconnect
        logger.debug(s"Init disconnect called for already disconnected $peer")
        val newState = InitializedDisconnectDone(
          clientConnectP = good.clientConnectP,
          clientDisconnectP = good.clientDisconnectP,
          versionMsgP = good.versionMsgP,
          verackMsgP = good.verackMsgP)
        newState
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
        val newState = InitializedDisconnect(initializing.clientConnectP,
                                             initializing.clientDisconnectP,
                                             initializing.versionMsgP,
                                             initializing.verackMsgP)
        newState
      case state: Normal =>
        val newState = InitializedDisconnect(state.clientConnectP,
                                             state.clientDisconnectP,
                                             state.versionMsgP,
                                             state.verackMsgP)
        newState
      case state: Waiting =>
        val newState = InitializedDisconnect(state.clientConnectP,
                                             state.clientDisconnectP,
                                             state.versionMsgP,
                                             state.verackMsgP)
        newState
    }
  }

  protected[networking] def disconnect(
      peer: Peer,
      p2pClientCallbacks: P2PClientCallbacks)(implicit
      system: ActorSystem): Future[PeerMessageReceiverState] = {
    import system.dispatcher
    logger.trace(s"Disconnecting with internalstate=${this}")
    this match {
      case bad @ (_: Disconnected | Preconnection |
          _: InitializedDisconnectDone | _: StoppedReconnect) =>
        val exn = new RuntimeException(
          s"Cannot disconnect from peer=${peer} when in state=${bad}")
        Future.failed(exn)
      case good: InitializedDisconnect =>
        val newState = InitializedDisconnectDone(
          clientConnectP = good.clientConnectP,
          clientDisconnectP = good.clientDisconnectP.success(()),
          versionMsgP = good.versionMsgP,
          verackMsgP = good.verackMsgP)
        p2pClientCallbacks.onDisconnect(peer).map(_ => newState)
      case good @ (_: Initializing | _: Normal | _: Waiting) =>
        val handleF: Future[Unit] = good match {
          case wait: Waiting =>
            onResponseTimeout(networkPayload = wait.responseFor,
                              peer = peer,
                              onQueryTimeout =
                                p2pClientCallbacks.onQueryTimeout)
              .map(_ => ())
          case wait: Initializing =>
            wait.initializationTimeoutCancellable.cancel()
            Future.unit
          case _ => Future.unit
        }

        logger.debug(s"Disconnected bitcoin peer=${peer}")
        val newState = Disconnected(
          clientConnectP = good.clientConnectP,
          clientDisconnectP = good.clientDisconnectP.success(()),
          versionMsgP = good.versionMsgP,
          verackMsgP = good.verackMsgP
        )

        for {
          _ <- handleF
          _ <- p2pClientCallbacks.onDisconnect(peer)
        } yield newState
    }
  }

  def onResponseTimeout(
      networkPayload: NetworkPayload,
      peer: Peer,
      onQueryTimeout: (ExpectsResponse, Peer) => Future[Unit])(implicit
      ec: ExecutionContext): Future[PeerMessageReceiverState] = {
    require(networkPayload.isInstanceOf[ExpectsResponse])
    logger.info(
      s"Handling response timeout for ${networkPayload.commandName} from $peer")

    //isn't this redundant? No, on response timeout may be called when not cancel timeout
    this match {
      case wait: Waiting => wait.expectedResponseCancellable.cancel()
      case _             =>
    }

    networkPayload match {
      case payload: ExpectsResponse =>
        logger.info(
          s"Response for ${payload.commandName} from $peer timed out in state $this")
        onQueryTimeout(payload, peer).map { _ =>
          this match {
            case _: Waiting if isConnected && isInitialized =>
              val newState =
                Normal(clientConnectP,
                       clientDisconnectP,
                       versionMsgP,
                       verackMsgP)
              newState
            case _: PeerMessageReceiverState => this
          }
        }
      case _ =>
        logger.error(
          s"onResponseTimeout called for ${networkPayload.commandName} which does not expect response")
        Future.successful(this)
    }
  }

  def handleExpectResponse(
      msg: NetworkPayload,
      peer: Peer,
      sendResponseTimeout: (Peer, NetworkPayload) => Future[Unit],
      onQueryTimeout: (ExpectsResponse, Peer) => Future[Unit])(implicit
      system: ActorSystem,
      nodeAppConfig: NodeAppConfig): Future[PeerMessageReceiverState] = {
    require(
      msg.isInstanceOf[ExpectsResponse],
      s"Cannot expect response for ${msg.commandName} from $peer as ${msg.commandName} does not expect a response.")
    import system.dispatcher
    this match {
      case good: Normal =>
        logger.debug(s"Handling expected response for ${msg.commandName}")
        val expectedResponseCancellable =
          system.scheduler.scheduleOnce(nodeAppConfig.queryWaitTime) {
            val responseTimeoutF =
              sendResponseTimeout(peer, msg)
            responseTimeoutF.failed.foreach(err =>
              logger.error(
                s"Failed to timeout waiting for response for peer=$peer",
                err))
          }
        val newState = Waiting(
          clientConnectP = good.clientConnectP,
          clientDisconnectP = good.clientDisconnectP,
          versionMsgP = good.versionMsgP,
          verackMsgP = good.verackMsgP,
          responseFor = msg,
          waitingSince = System.currentTimeMillis(),
          expectedResponseCancellable = expectedResponseCancellable
        )
        Future.successful(newState)
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
        onResponseTimeout(msg, peer, onQueryTimeout = onQueryTimeout)
    }
  }

  def stopReconnect(peer: Peer): PeerMessageReceiverState = {
    this match {
      case Preconnection =>
        //when retry, state should be back to preconnection
        val newState = StoppedReconnect(clientConnectP,
                                        clientDisconnectP,
                                        versionMsgP,
                                        verackMsgP)
        newState
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
}

object PeerMessageReceiverState {

  /** Represents a [[org.bitcoins.node.networking.peer.PeerMessageReceiverState PeerMessageReceiverState]]
    * where the peer is not connected to the p2p network
    */
  final case object Preconnection extends PeerMessageReceiverState {
    def clientConnectP: Promise[P2PClient] = Promise[P2PClient]()

    //should this be completed since the client is disconnected???
    def clientDisconnectP: Promise[Unit] = Promise[Unit]()
    def versionMsgP: Promise[VersionMessage] = Promise[VersionMessage]()
    def verackMsgP: Promise[VerAckMessage.type] = Promise[VerAckMessage.type]()

    /** Converts [[org.bitcoins.node.networking.peer.PeerMessageReceiverState.Preconnection Preconnection]] to [[Initializing]] */
    def toInitializing(
        client: P2PClient,
        timeout: Cancellable): Initializing = {
      val p = clientConnectP
      p.success(client)
      Initializing(
        clientConnectP = p,
        clientDisconnectP = clientDisconnectP,
        versionMsgP = versionMsgP,
        verackMsgP = verackMsgP,
        waitingSince = System.currentTimeMillis(),
        initializationTimeoutCancellable = timeout
      )
    }
  }

  /** Means that our [[org.bitcoins.node.networking.peer.PeerMessageReceiver]]
    * is still going through the initilization process. This means
    * we still need to receive a [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] or [[org.bitcoins.core.p2p.VerAckMessage VerAckMessage]]
    * from our peer on the p2p network
    */
  case class Initializing(
      clientConnectP: Promise[P2PClient],
      clientDisconnectP: Promise[Unit],
      versionMsgP: Promise[VersionMessage],
      verackMsgP: Promise[VerAckMessage.type],
      waitingSince: Long,
      initializationTimeoutCancellable: Cancellable
  ) extends PeerMessageReceiverState {
    require(
      isConnected,
      "We cannot have a PeerMessageReceiverState.Initializng if we are not connected")

    /** Helper method to modifing the state of [[org.bitcoins.node.networking.peer.PeerMessageReceiverState.Initializing]]
      * when we receive a [[org.bitcoins.core.p2p.VersionMessage VersionMessage]]. This completes versoinMsgP
      * @return
      */
    def withVersionMsg(versionMsg: VersionMessage): Initializing = {
      PeerMessageReceiverState.Initializing(
        clientConnectP = clientConnectP,
        clientDisconnectP = clientDisconnectP,
        versionMsgP = versionMsgP.success(versionMsg),
        verackMsgP = verackMsgP,
        waitingSince = waitingSince,
        initializationTimeoutCancellable = initializationTimeoutCancellable
      )
    }

    /** Completes the verack message promise and transitions
      * our [[org.bitcoins.node.networking.peer.PeerMessageReceiverState PeerMessageReceiverState]] to [[org.bitcoins.node.networking.peer.PeerMessageReceiverState.Normal PeerMessageReceiverState.Normal]]
      */
    def toNormal(verAckMessage: VerAckMessage.type): Normal = {
      initializationTimeoutCancellable.cancel()
      Normal(
        clientConnectP = clientConnectP,
        clientDisconnectP = clientDisconnectP,
        versionMsgP = versionMsgP,
        verackMsgP = verackMsgP.success(verAckMessage)
      )
    }

    override def toString: String = "Initializing"
  }

  /** This represents a [[org.bitcoins.node.networking.peer.PeerMessageReceiverState]]
    * where the peer has been fully initialized and is ready to send messages to
    * the peer on the network
    */
  case class Normal(
      clientConnectP: Promise[P2PClient],
      clientDisconnectP: Promise[Unit],
      versionMsgP: Promise[VersionMessage],
      verackMsgP: Promise[VerAckMessage.type]
  ) extends PeerMessageReceiverState {
    require(
      isConnected,
      s"We cannot have a PeerMessageReceiverState.Normal if the Peer is not connected")
    require(
      isInitialized,
      s"We cannot have a PeerMessageReceiverState.Normal if the Peer is not initialized")

    override def toString: String = "Normal"
  }

  /** The state for when we initialized as disconnect from our peer */
  case class InitializedDisconnect(
      clientConnectP: Promise[P2PClient],
      clientDisconnectP: Promise[Unit],
      versionMsgP: Promise[VersionMessage],
      verackMsgP: Promise[VerAckMessage.type])
      extends PeerMessageReceiverState {
    require(
      isConnected,
      s"Cannot have a PeerMessageReceiverState.InitializeDisconnect when peer is not connected"
    )

    override def toString: String = "InitializedDisconnect"
  }

  /** State when waiting for response to a message of type [[org.bitcoins.core.p2p.ExpectsResponse]]. Other messages
    * are still processed and receiver will continue waiting until timeout.
    */
  case class Waiting(
      clientConnectP: Promise[P2PClient],
      clientDisconnectP: Promise[Unit],
      versionMsgP: Promise[VersionMessage],
      verackMsgP: Promise[VerAckMessage.type],
      responseFor: NetworkPayload,
      waitingSince: Long,
      expectedResponseCancellable: Cancellable)
      extends PeerMessageReceiverState {
    override def toString: String = "Waiting"
  }

  case class StoppedReconnect(
      clientConnectP: Promise[P2PClient],
      clientDisconnectP: Promise[Unit],
      versionMsgP: Promise[VersionMessage],
      verackMsgP: Promise[VerAckMessage.type])
      extends PeerMessageReceiverState {
    override def toString: String = "StoppedReconnect"

    assert(
      !isConnected, //since the promise is not complete both isConnected and isDisconnected is false here
      s"Cannot have a PeerMessageReceiverState.StoppedReconnect when peer is connected"
    )
    assert(
      !isInitialized,
      s"Cannot have a PeerMessageReceiverState.StoppedReconnect when peer is initialised")
  }

  /** This means we initialized a disconnection from the peer
    * and it is successfully completed now.
    * This is different than the [[Disconnected]] state as it is
    * useful for situations where we don't want to reconnect
    * because we initialized the disconnection
    */
  case class InitializedDisconnectDone(
      clientConnectP: Promise[P2PClient],
      clientDisconnectP: Promise[Unit],
      versionMsgP: Promise[VersionMessage],
      verackMsgP: Promise[VerAckMessage.type])
      extends PeerMessageReceiverState {
    require(
      isDisconnected,
      s"We cannot have a PeerMessageReceiverState.InitializedDisconnectDone if the Peer is connected")

    override def toString: String = "InitializedDisconnect"
  }

  /** Means we are disconnected from a peer. This is different
    * than [[InitializedDisconnectDone]] because this means
    * the peer disconnected us
    */
  case class Disconnected(
      clientConnectP: Promise[P2PClient],
      clientDisconnectP: Promise[Unit],
      versionMsgP: Promise[VersionMessage],
      verackMsgP: Promise[VerAckMessage.type])
      extends PeerMessageReceiverState {
    require(
      isDisconnected,
      "We cannot be in the disconnected state if a peer is not disconnected")

    override def toString: String = "Disconnected"

  }

  def fresh(): PeerMessageReceiverState.Preconnection.type = {
    PeerMessageReceiverState.Preconnection
  }

}
