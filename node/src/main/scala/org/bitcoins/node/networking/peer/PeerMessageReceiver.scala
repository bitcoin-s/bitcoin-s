package org.bitcoins.node.networking.peer

import akka.actor.ActorRefFactory
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.p2p.NetworkMessage
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.core.p2p._
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.PeerMessageReceiverState.{
  Disconnected,
  Initializing,
  Normal,
  Preconnection
}

import scala.util.{Failure, Success, Try}
import org.bitcoins.node.SpvNodeCallbacks

/**
  * Responsible for receiving messages from a peer on the
  * p2p network. This is called by [[org.bitcoins.rpc.client.common.Client Client]] when doing the p2p
  * handshake and during the [[PeerMessageReceiverState.Normal Normal]]
  * operations. This is the entry point for handling all received
  * [[org.bitcoins.core.p2p.NetworkMessage NetworkMessage]]
  */
class PeerMessageReceiver(
    state: PeerMessageReceiverState,
    callbacks: SpvNodeCallbacks
)(
    implicit ref: ActorRefFactory,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig)
    extends BitcoinSLogger {

  import ref.dispatcher

  //TODO: Really bad to just modify this internal state
  //not async safe at all
  private var internalState: PeerMessageReceiverState = state

  /** The peer we are connected to. */
  private var peerOpt: Option[Peer] = None

  /** This method is called when we have received
    * a [[akka.io.Tcp.Connected]] message from our peer
    * This means we have opened a Tcp connection,
    * but have NOT started the handshake
    * This method will initiate the handshake
    */
  protected[networking] def connect(client: P2PClient): Try[Unit] = {

    internalState match {
      case bad @ (_: Initializing | _: Normal | _: Disconnected) =>
        Failure(
          new RuntimeException(s"Cannot call connect when in state=${bad}")
        )
      case Preconnection =>
        peerOpt = Some(client.peer)

        logger.info(s"Connection established with peer=${peerOpt.get}")

        val newState = Preconnection.toInitializing(client)

        val _ = toState(newState)

        val peerMsgSender = PeerMessageSender(client)

        peerMsgSender.sendVersionMessage()

        Success(())
    }
  }

  protected[networking] def disconnect(): Try[Unit] = {

    internalState match {
      case bad @ (_: Initializing | _: Disconnected | Preconnection) =>
        Failure(
          new RuntimeException(
            s"Cannot disconnect from peer=${peerOpt.get} when in state=${bad}")
        )

      case good: Normal =>
        logger.debug(s"Disconnected bitcoin peer=${peerOpt.get}")
        val newState = Disconnected(
          clientConnectP = good.clientConnectP,
          clientDisconnectP = good.clientDisconnectP.success(()),
          versionMsgP = good.versionMsgP,
          verackMsgP = good.verackMsgP
        )

        val _ = toState(newState)
        Success(())
    }
  }

  def isConnected: Boolean = internalState.isConnected

  def isDisconnected: Boolean = internalState.isDisconnected

  def hasReceivedVersionMsg: Boolean =
    internalState.hasReceivedVersionMsg.isCompleted

  def hasReceivedVerackMsg: Boolean =
    internalState.hasReceivedVerackMsg.isCompleted

  def isInitialized: Boolean = internalState.isInitialized

  def handleNetworkMessageReceived(
      networkMsgRecv: PeerMessageReceiver.NetworkMessageReceived): Unit = {

    val client = networkMsgRecv.client

    //create a way to send a response if we need too
    val peerMsgSender = PeerMessageSender(client)

    logger.debug(
      s"Received message=${networkMsgRecv.msg.header.commandName} from peer=${client.peer} ")
    networkMsgRecv.msg.payload match {
      case controlPayload: ControlPayload =>
        handleControlPayload(payload = controlPayload, sender = peerMsgSender)
        ()
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
      sender: PeerMessageSender): Unit = {
    val dataMsgHandler = new DataMessageHandler(callbacks)
    //else it means we are receiving this data payload from a peer,
    //we need to handle it
    dataMsgHandler.handleDataPayload(payload, sender)
    ()
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
      sender: PeerMessageSender): Try[Unit] = {
    payload match {

      case versionMsg: VersionMessage =>
        logger.trace(
          s"Received versionMsg=${versionMsg}from peer=${peerOpt.get}")

        internalState match {
          case bad @ (_: Disconnected | _: Normal | Preconnection) =>
            Failure(
              new RuntimeException(
                s"Cannot handle version message while in state=${bad}"))

          case good: Initializing =>
            internalState = good.withVersionMsg(versionMsg)

            sender.sendVerackMessage()

            //we want peers to just send us headers
            //we don't want to have to request them manually
            sender.sendHeadersMessage()

            Success(())
        }

      case VerAckMessage =>
        internalState match {
          case bad @ (_: Disconnected | _: Normal | Preconnection) =>
            Failure(
              new RuntimeException(
                s"Cannot handle version message while in state=${bad}"))

          case good: Initializing =>
            internalState = good.toNormal(VerAckMessage)
            Success(())
        }

      case ping: PingMessage =>
        sender.sendPong(ping)
        Success(())
      case SendHeadersMessage =>
        //not implemented as of now
        Success(())
      case _: AddrMessage =>
        Success(())
      case _ @(_: FilterAddMessage | _: FilterLoadMessage |
          FilterClearMessage) =>
        Success(())
      case _ @(GetAddrMessage | _: PongMessage) =>
        Success(())
      case _: RejectMessage =>
        Success(())
      case _: FeeFilterMessage =>
        Success(())
    }
  }

  private def toState(state: PeerMessageReceiverState): Unit = {
    logger.debug(
      s"PeerMessageReceiver changing state, oldState=$internalState, newState=$state")
    internalState = state
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
      callbacks: SpvNodeCallbacks = SpvNodeCallbacks.empty)(
      implicit ref: ActorRefFactory,
      nodeAppConfig: NodeAppConfig,
      chainAppConfig: ChainAppConfig
  ): PeerMessageReceiver = {
    new PeerMessageReceiver(state, callbacks)
  }

  def newReceiver(callbacks: SpvNodeCallbacks = SpvNodeCallbacks.empty)(
      implicit nodeAppConfig: NodeAppConfig,
      chainAppConfig: ChainAppConfig,
      ref: ActorRefFactory): PeerMessageReceiver = {
    new PeerMessageReceiver(state = PeerMessageReceiverState.fresh(), callbacks)
  }
}
