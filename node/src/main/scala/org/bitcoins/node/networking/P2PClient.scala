package org.bitcoins.node.networking

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props, Terminated}
import akka.event.LoggingReceive
import akka.io.Tcp.SO.KeepAlive
import akka.io.{IO, Tcp}
import akka.util.{ByteString, CompactByteString, Timeout}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.p2p.{NetworkHeader, NetworkMessage, NetworkPayload}
import org.bitcoins.core.util.{FutureUtil, NetworkUtil}
import org.bitcoins.node.P2PLogger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient.NodeCommand
import org.bitcoins.node.networking.peer.PeerMessageReceiver.NetworkMessageReceived
import org.bitcoins.node.networking.peer.PeerMessageReceiverState.{
  Disconnected,
  Initializing,
  Normal
}
import org.bitcoins.node.networking.peer.{
  PeerMessageReceiver,
  PeerMessageReceiverState
}
import org.bitcoins.node.util.BitcoinSNodeUtil
import org.bitcoins.tor.Socks5Connection.{Socks5Connect, Socks5Connected}
import org.bitcoins.tor.{Socks5Connection, Socks5ProxyParams}
import scodec.bits.ByteVector

import java.net.InetSocketAddress
import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util._

/** This actor is responsible for creating a connection,
  * relaying messages and closing a connection to our peer on
  * the P2P network. This is the actor that directly interacts
  * with the p2p network. It's responsibly is to deal with low
  * level .TCP messages.
  *
  * If the client receives a [[org.bitcoins.core.p2p.NetworkMessage NetworkMessage]], from a
  * [[org.bitcoins.node.networking.peer.PeerMessageSender PeerMessageSender]]
  * it serializes the message to it to a [[akka.util.ByteString]] and then
  * sends it to the internal `manager` which streams the data to our peer
  * on the Bitcoin network.
  *
  * If the client receives a [[Tcp.Received]] message, it means we have received
  * a message from our peer on the Bitcoin P2P network. This means we try to parse
  * the bytes into a [[org.bitcoins.core.p2p.NetworkMessage NetworkMessage]].
  * If we successfully parse the message we relay that message to the
  * [[org.bitcoins.node.networking.peer.PeerMessageSender PeerMessageSender]]
  * that created the Client Actor.
  *
  * In this class you will see a 'unalignedBytes' value passed around in a lot of methods.
  * This is because we cannot assume that a Bitcoin P2P message aligns with a TCP packet.
  * For instance, a large block message (up to 4MB in size)
  * CANNOT fit in a single TCP packet. This means we must cache
  * the bytes and wait for the rest of them to be sent.
  *
  * @param initPeerMsgHandlerReceiver The place we send messages that we successfully parsed
  *                               from our peer on the P2P network. This is mostly likely
  *                               a [[org.bitcoins.node.networking.peer.PeerMessageSender]]
  */
case class P2PClientActor(
    peer: Peer,
    initPeerMsgHandlerReceiver: PeerMessageReceiver,
    onReconnect: () => Future[Unit]
)(implicit config: NodeAppConfig)
    extends Actor
    with P2PLogger {

  private case object ReconnectCommand extends NodeCommand

  private var currentPeerMsgHandlerRecv = initPeerMsgHandlerReceiver

  private var reconnectHandlerOpt: Option[() => Future[Unit]] = None

  private val maxReconnectionTries = 16

  private var reconnectionTry = 0

  private val reconnectionDelay = 500.millis

  /** The parameters for the network we are connected to
    */
  private val network: NetworkParameters = config.network

  private val timeout = 1000.seconds

  /** The manager is an actor that handles the underlying low level I/O resources (selectors, channels)
    * and instantiates workers for specific tasks, such as listening to incoming connections.
    */
  def manager: ActorRef = IO(Tcp)(context.system)

  /** TODO: this comment seems wrong?
    *
    * This actor signifies the node we are connected to on the p2p network
    * This is the context we are in after we received a [[Tcp.Connected]] message
    */
  private def awaitNetworkRequest(
      peerConnection: ActorRef,
      unalignedBytes: ByteVector): Receive =
    LoggingReceive {
      case message: NetworkMessage =>
        sendNetworkMessage(message, peerConnection)
      case payload: NetworkPayload =>
        val networkMsg = NetworkMessage(network, payload)
        self.forward(networkMsg)
      case message: Tcp.Event =>
        val newUnalignedBytes =
          handleEvent(message, peerConnection, unalignedBytes)
        context.become(awaitNetworkRequest(peerConnection, newUnalignedBytes))
      case P2PClient.CloseCommand =>
        logger.info(s"disconnecting from peer $peer")
        currentPeerMsgHandlerRecv =
          currentPeerMsgHandlerRecv.initializeDisconnect()
        peerConnection ! Tcp.Close
      case metaMsg: P2PClient.MetaMsg =>
        sender() ! handleMetaMsg(metaMsg)
      case Terminated(actor) if actor == peerConnection =>
        reconnect()
    }

  override def receive: Receive = LoggingReceive {
    case P2PClient.ConnectCommand =>
      connect()
    case metaMsg: P2PClient.MetaMsg =>
      sender() ! handleMetaMsgDisconnected(metaMsg)
  }

  def reconnecting: Receive = LoggingReceive {
    case ReconnectCommand =>
      logger.info(s"reconnecting to ${peer.socket}")
      reconnectHandlerOpt = Some(onReconnect)
      connect()
    case metaMsg: P2PClient.MetaMsg =>
      sender() ! handleMetaMsgDisconnected(metaMsg)
  }

  def connecting(proxyParams: Option[Socks5ProxyParams]): Receive =
    LoggingReceive {
      case Tcp.CommandFailed(c: Tcp.Connect) =>
        val peerOrProxyAddress = c.remoteAddress
        logger.error(
          s"connection failed to ${peerOrProxyAddress} ${proxyParams}")
        reconnect()

      case event @ Tcp.Connected(peerOrProxyAddress, _) =>
        val connection = sender()
        proxyParams match {
          case Some(proxyParams) =>
            val proxyAddress = peerOrProxyAddress
            val remoteAddress = peer.socket
            logger.info(s"connected to SOCKS5 proxy ${proxyAddress}")
            logger.info(
              s"connecting to ${remoteAddress} via SOCKS5 ${proxyAddress}")
            val proxy =
              context.actorOf(Socks5Connection.props(
                                sender(),
                                Socks5ProxyParams.proxyCredentials(proxyParams),
                                Socks5Connect(remoteAddress)),
                              "Socks5Connection")
            context watch proxy
            context become socks5Connecting(event,
                                            proxy,
                                            remoteAddress,
                                            proxyAddress)
          case None =>
            val peerAddress = peerOrProxyAddress
            logger.info(s"connected to ${peerAddress}")
            context watch connection
            val _ = handleEvent(event, connection, ByteVector.empty)
        }
      case metaMsg: P2PClient.MetaMsg =>
        sender() ! handleMetaMsgDisconnected(metaMsg)
    }

  def socks5Connecting(
      event: Tcp.Connected,
      proxy: ActorRef,
      remoteAddress: InetSocketAddress,
      proxyAddress: InetSocketAddress): Receive = LoggingReceive {
    case Tcp.CommandFailed(_: Socks5Connect) =>
      logger.error(
        s"connection failed to ${remoteAddress} via SOCKS5 ${proxyAddress}")
      reconnect()
    case Socks5Connected(_) =>
      logger.info(
        s"connected to ${remoteAddress} via SOCKS5 proxy ${proxyAddress}")
      val _ = handleEvent(event, proxy, ByteVector.empty)
    case Terminated(actor) if actor == proxy =>
      reconnect()
    case metaMsg: P2PClient.MetaMsg =>
      sender() ! handleMetaMsgDisconnected(metaMsg)
  }

  override def unhandled(message: Any): Unit = message match {
    case payload: NetworkPayload =>
      logger.error(
        s"Cannot send a message to our peer when we are not connected! payload=${payload} peer=${peer}")
    case _ =>
      logger.warn(s"unhandled message=$message")
  }

  private def connect(): Unit = {
    val (peerOrProxyAddress, proxyParams) =
      peer.socks5ProxyParams match {
        case Some(proxyParams) =>
          val host = peer.socket.getHostString
          if (!NetworkUtil.isLocalhost(host)) {
            val proxyAddress = proxyParams.address
            logger.info(s"connecting to SOCKS5 proxy $proxyAddress")
            (proxyAddress, Some(proxyParams))
          } else {
            val remoteAddress = peer.socket
            logger.info(s"connecting to $remoteAddress")
            (peer.socket, None)
          }
        case None =>
          val remoteAddress = peer.socket
          logger.info(s"connecting to $remoteAddress")
          (peer.socket, None)
      }
    manager ! Tcp.Connect(peerOrProxyAddress,
                          timeout = Some(20.seconds),
                          options = KeepAlive(true) :: Nil,
                          pullMode = true)
    context become connecting(proxyParams)
  }

  private def reconnect(): Unit = {
    currentPeerMsgHandlerRecv.state match {
      case _: PeerMessageReceiverState.InitializedDisconnect =>
        logger.warn(
          s"Ignoring reconnection attempts as we initialized disconnect from peer=$peer")
      case PeerMessageReceiverState.Preconnection | _: Initializing |
          _: Normal | _: Disconnected =>
        currentPeerMsgHandlerRecv = initPeerMsgHandlerReceiver

        if (reconnectionTry >= maxReconnectionTries) {
          logger.error("Exceeded maximum number of reconnection attempts")
          context.stop(self)
        } else {
          val delay = reconnectionDelay * (1 << reconnectionTry)
          reconnectionTry = reconnectionTry + 1

          import context.dispatcher
          context.system.scheduler.scheduleOnce(delay)(self ! ReconnectCommand)

          context.become(reconnecting)
        }
    }
  }

  /** This function is responsible for handling a [[Tcp.Event]] algebraic data type
    */
  private def handleEvent(
      event: Tcp.Event,
      peerConnection: ActorRef,
      unalignedBytes: ByteVector): ByteVector = {
    event match {
      case Tcp.Bound(localAddress) =>
        logger.debug(
          s"Actor is now bound to the local address: ${localAddress}")
        context.parent ! Tcp.Bound(localAddress)

        unalignedBytes
      case Tcp.CommandFailed(command) =>
        logger.debug(s"Client Command failed: ${command}")

        unalignedBytes
      case Tcp.Connected(remote, local) =>
        logger.debug(s"Tcp connection to: ${remote}")
        logger.debug(s"Local: ${local}")

        //this is what registers a actor to send all byte messages to that is
        //received from our peer. Since we are using 'self' that means
        //our bitcoin peer will send all messages to this actor.
        peerConnection ! Tcp.Register(self)
        peerConnection ! Tcp.ResumeReading

        currentPeerMsgHandlerRecv =
          currentPeerMsgHandlerRecv.connect(P2PClient(self, peer))
        context.become(awaitNetworkRequest(peerConnection, unalignedBytes))
        unalignedBytes

      case closeCmd @ (Tcp.ConfirmedClosed | Tcp.Closed | Tcp.Aborted |
          Tcp.PeerClosed | Tcp.ErrorClosed(_)) =>
        logger.info(s"We've been disconnected by $peer command=${closeCmd}")
        currentPeerMsgHandlerRecv.disconnect()
        unalignedBytes

      case Tcp.Received(byteString: ByteString) =>
        val byteVec = ByteVector(byteString.toArray)
        logger.debug(s"Received ${byteVec.length} TCP bytes")
        logger.trace(s"Received TCP bytes: ${byteVec.toHex}")
        logger.trace {
          val post =
            if (unalignedBytes.isEmpty) "None"
            else unalignedBytes.toHex
          s"Unaligned bytes: $post"
        }

        if (unalignedBytes.isEmpty) {
          peerConnection ! Tcp.ResumeReading
        }

        //we need to aggregate our previous 'unalignedBytes' with the new message
        //we just received from our peer to hopefully be able to parse full messages
        val bytes: ByteVector = unalignedBytes ++ byteVec
        logger.trace(s"Bytes for message parsing: ${bytes.toHex}")
        val (messages, newUnalignedBytes) =
          P2PClient.parseIndividualMessages(bytes)

        logger.debug {
          val length = messages.length
          val suffix = if (length == 0) "" else s": ${messages.mkString(", ")}"

          s"Parsed $length message(s) from bytes$suffix"
        }
        logger.debug(s"Unaligned bytes after this: ${newUnalignedBytes.length}")
        if (newUnalignedBytes.nonEmpty) {
          logger.trace(s"Unaligned bytes: ${newUnalignedBytes.toHex}")
        }

        val f: (
            PeerMessageReceiver,
            NetworkMessage) => Future[PeerMessageReceiver] = {
          case (peerMsgRecv: PeerMessageReceiver, m: NetworkMessage) =>
            logger.trace(s"Processing message=${m}")
            val msg = NetworkMessageReceived(m, P2PClient(self, peer))
            if (peerMsgRecv.isConnected) {
              peerMsgRecv.handleNetworkMessageReceived(msg)
            } else {
              Future.successful(peerMsgRecv)
            }
        }

        logger.trace(s"About to process ${messages.length} messages")
        val newMsgReceiverF =
          FutureUtil.foldLeftAsync(currentPeerMsgHandlerRecv, messages)(f)(
            context.dispatcher)

        val newMsgReceiver = Await.result(newMsgReceiverF, timeout)
        currentPeerMsgHandlerRecv = newMsgReceiver
        if (currentPeerMsgHandlerRecv.isInitialized) {
          reconnectionTry = 0
          reconnectHandlerOpt.foreach(_())
          reconnectHandlerOpt = None
        }
        peerConnection ! Tcp.ResumeReading
        newUnalignedBytes
    }
  }

  /** Returns the current state of our peer given the [[P2PClient.MetaMsg meta message]]
    */
  private def handleMetaMsg(metaMsg: P2PClient.MetaMsg): Boolean = {
    metaMsg match {
      case P2PClient.IsConnected    => currentPeerMsgHandlerRecv.isConnected
      case P2PClient.IsInitialized  => currentPeerMsgHandlerRecv.isInitialized
      case P2PClient.IsDisconnected => currentPeerMsgHandlerRecv.isDisconnected
    }
  }

  private def handleMetaMsgDisconnected(metaMsg: P2PClient.MetaMsg): Boolean = {
    metaMsg match {
      case P2PClient.IsConnected    => false
      case P2PClient.IsInitialized  => false
      case P2PClient.IsDisconnected => true
    }
  }

  /** Sends a network request to our peer on the network
    */
  private def sendNetworkMessage(
      message: NetworkMessage,
      peerConnection: ActorRef): Unit = {

    val byteMessage = CompactByteString(message.bytes.toArray)
    peerConnection ! Tcp.Write(byteMessage)
    peerConnection ! Tcp.ResumeReading
  }

}

case class P2PClient(actor: ActorRef, peer: Peer) extends P2PLogger {
  import akka.pattern.ask

  def isConnected()(implicit
      timeout: Timeout,
      ec: ExecutionContext): Future[Boolean] = {
    val isConnectedF = actor.ask(P2PClient.IsConnected).mapTo[Boolean]
    isConnectedF.recoverWith { case _: Throwable =>
      Future.successful(false)
    }
  }

  def isInitialized()(implicit
      timeout: Timeout,
      ec: ExecutionContext): Future[Boolean] = {
    val isInitF = actor.ask(P2PClient.IsInitialized).mapTo[Boolean]
    isInitF.recoverWith { case _: Throwable =>
      Future.successful(false)
    }
  }

  def isDisconnected()(implicit
      timeout: Timeout,
      ec: ExecutionContext): Future[Boolean] = {
    val isDisconnect: Future[Boolean] =
      actor.ask(P2PClient.IsDisconnected).mapTo[Boolean]

    //this future can be failed, as we stop the P2PClientActor if we send a disconnect
    //if that actor has been killed, the peer _has_ to have been disconnected
    isDisconnect.recoverWith { case _: Throwable =>
      Future.successful(true)
    }
  }
}

object P2PClient extends P2PLogger {

  sealed trait NodeCommand
  case object ConnectCommand extends NodeCommand
  case object CloseCommand extends NodeCommand

  /** A message hierarchy that canbe sent to [[P2PClientActor P2P Client Actor]]
    * to query about meta information of a peer
    */
  sealed trait MetaMsg

  /** A message that can be sent to [[P2PClient p2p client]] that returns true
    * if the peer is connected, false if not
    */
  final case object IsConnected extends MetaMsg

  /** A message that can be sent to [[P2PClient p2p client]] that returns true
    * if the peer is initialized (p2p handshake complete), false if not
    */
  final case object IsInitialized extends MetaMsg

  /** A message that can be sent to [[P2PClient p2p client]] that returns true
    * if the peer is disconnected, false otherwise
    */
  final case object IsDisconnected extends MetaMsg

  def props(
      peer: Peer,
      peerMsgHandlerReceiver: PeerMessageReceiver,
      onReconnect: () => Future[Unit])(implicit
      config: NodeAppConfig
  ): Props =
    Props(classOf[P2PClientActor],
          peer,
          peerMsgHandlerReceiver,
          onReconnect,
          config)

  def apply(
      context: ActorRefFactory,
      peer: Peer,
      peerMessageReceiver: PeerMessageReceiver,
      onReconnect: () => Future[Unit])(implicit
      config: NodeAppConfig): P2PClient = {
    val actorRef = context.actorOf(
      props = props(peer, peerMessageReceiver, onReconnect),
      name = BitcoinSNodeUtil.createActorName(getClass))

    P2PClient(actorRef, peer)
  }

  /** Akka sends messages as one byte stream. There is not a 1 to 1 relationship between byte streams received and
    * bitcoin protocol messages. This function parses our byte stream into individual network messages
    *
    * @param bytes the bytes that need to be parsed into individual messages
    * @return the parsed [[NetworkMessage]]'s and the unaligned bytes that did not parse to a message
    */
  private[bitcoins] def parseIndividualMessages(
      bytes: ByteVector): (Vector[NetworkMessage], ByteVector) = {
    @tailrec
    def loop(
        remainingBytes: ByteVector,
        accum: Vector[NetworkMessage]): (Vector[NetworkMessage], ByteVector) = {
      if (remainingBytes.length <= 0) {
        (accum, remainingBytes)
      } else {
        val headerTry = Try(
          NetworkHeader.fromBytes(remainingBytes.take(NetworkHeader.bytesSize)))
        headerTry match {
          case Success(header) =>
            val payloadBytes = remainingBytes
              .drop(NetworkHeader.bytesSize)
              .take(header.payloadSize.toInt)

            val newRemainingBytes =
              remainingBytes.drop(NetworkHeader.bytesSize + payloadBytes.size)

            // If it's a message type we know, try to parse it
            if (NetworkPayload.commandNames.contains(header.commandName)) {
              Try(NetworkMessage(header.bytes ++ payloadBytes)) match {
                case Success(message) =>
                  logger.trace(
                    s"Parsed a message=${message.header.commandName} from bytes, continuing with remainingBytes=${newRemainingBytes.length}")

                  loop(newRemainingBytes, accum :+ message)
                case Failure(_) =>
                  // Can't parse message yet, we need to wait for more bytes
                  (accum, remainingBytes)
              }
            } else if (payloadBytes.size == header.payloadSize.toInt) { // If we've received the entire unknown message
              logger.info(
                s"Received unknown network message ${header.commandName}")
              loop(newRemainingBytes, accum)
            } else {
              // If we can't parse the entire unknown message, continue on until we can
              // so we properly skip it
              (accum, remainingBytes)
            }
          case Failure(exc) =>
            logger.trace(
              s"Failed to parse network message $remainingBytes, could be because TCP frame isn't aligned: $exc")

            //this case means that our TCP frame was not aligned with bitcoin protocol
            //return the unaligned bytes so we can apply them to the next tcp frame of bytes we receive
            //http://stackoverflow.com/a/37979529/967713
            (accum, remainingBytes)
        }
      }
    }
    val (messages, remainingBytes) = loop(bytes, Vector.empty)
    (messages, remainingBytes)
  }

}
