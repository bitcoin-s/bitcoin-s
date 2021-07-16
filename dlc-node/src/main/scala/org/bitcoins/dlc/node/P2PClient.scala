package org.bitcoins.dlc.node

import akka.actor._
import akka.event.LoggingReceive
import akka.io.Tcp.SO.KeepAlive
import akka.io._
import akka.util._
import grizzled.slf4j.Logging
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.dlc.node.peer.PeerMessageReceiver.LnMessageReceived
import org.bitcoins.dlc.node.peer._
import org.bitcoins.tor.Socks5Connection._
import org.bitcoins.tor._
import scodec.bits.ByteVector

import java.net.InetSocketAddress
import scala.annotation.tailrec
import scala.concurrent._
import scala.concurrent.duration.DurationInt
import scala.util._

/** This actor is responsible for creating a connection,
  * relaying messages and closing a connection to our peer on
  * the P2P network. This is the actor that directly interacts
  * with the p2p network. It's responsibly is to deal with low
  * level .TCP messages.
  *
  * If the client receives a [[org.bitcoins.core.protocol.tlv.LnMessage LnMessage]], from a
  * [[org.bitcoins.dlc.node.peer.PeerMessageSender PeerMessageSender]]
  * it serializes the message to it to a [[akka.util.ByteString]] and then
  * sends it to the internal `manager` which streams the data to our peer
  * on the Bitcoin network.
  *
  * If the client receives a [[Tcp.Received]] message, it means we have received
  * a message from our peer on the Bitcoin P2P network. This means we try to parse
  * the bytes into a [[org.bitcoins.core.protocol.tlv.LnMessage LnMessage]].
  * If we successfully parse the message we relay that message to the
  * [[org.bitcoins.dlc.node.peer.PeerMessageSender PeerMessageSender]]
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
  *                               a [[org.bitcoins.dlc.node.peer.PeerMessageSender]]
  */
case class P2PClientActor(
    peer: Peer,
    initPeerMsgHandlerReceiver: PeerMessageReceiver)
    extends Actor
    with Logging {

  private var currentPeerMsgHandlerRecv: PeerMessageReceiver =
    initPeerMsgHandlerReceiver

  /** The manager is an actor that handles the underlying low level I/O resources (selectors, channels)
    * and instantiates workers for specific tasks, such as listening to incoming connections.
    */
  def manager: ActorRef = IO(Tcp)(context.system)

  private val timeout = 1000.seconds

  /** TODO: this comment seems wrong?
    *
    * This actor signifies the node we are connected to on the p2p network
    * This is the context we are in after we received a [[Tcp.Connected]] message
    */
  private def awaitNetworkRequest(
      peerConnection: ActorRef,
      unalignedBytes: ByteVector): Receive = {
    LoggingReceive {
      case lnMessage: LnMessage[TLV] =>
        sendLnMessage(lnMessage, peerConnection)
      case tlv: TLV =>
        val lnMessage = LnMessage[TLV](tlv)
        self.forward(lnMessage)
      case message: Tcp.Message =>
        val newUnalignedBytes =
          handleTcpMessage(message, peerConnection, unalignedBytes)
        context.become(awaitNetworkRequest(peerConnection, newUnalignedBytes))
      case metaMsg: P2PClient.MetaMsg =>
        sender() ! handleMetaMsg(metaMsg)
      case Terminated(actor) if actor == peerConnection =>
        context stop self
    }
  }

  def receive: Receive = LoggingReceive {
    case P2PClient.BindCommand =>
      manager ! Tcp.Bind(self, peer.socket)
    case P2PClient.ConnectCommand =>
      val (peerOrProxyAddress, proxyParams) =
        peer.socks5ProxyParams match {
          case Some(proxyParams) =>
            val proxyAddress = proxyParams.address
            logger.info(s"connecting to SOCKS5 proxy $proxyAddress")
            (proxyAddress, Some(proxyParams))
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
    case metaMsg: P2PClient.MetaMsg =>
      sender() ! handleMetaMsgDisconnected(metaMsg)
  }

  def connecting(proxyParams: Option[Socks5ProxyParams]): Receive =
    LoggingReceive {
      case b: Tcp.Bound =>
        context.parent ! b
      case Tcp.CommandFailed(c: Tcp.Connect) =>
        val peerOrProxyAddress = c.remoteAddress
        logger.error(s"connection failed to $peerOrProxyAddress $proxyParams")

      case event @ Tcp.Connected(peerOrProxyAddress, _) =>
        val connection = sender()
        proxyParams match {
          case Some(proxyParams) =>
            val proxyAddress = peerOrProxyAddress
            val remoteAddress = peer.socket
            logger.info(s"connected to SOCKS5 proxy $proxyAddress")
            logger.info(
              s"connecting to $remoteAddress via SOCKS5 $proxyAddress")
            val proxy =
              context.actorOf(Socks5Connection.props(
                                sender(),
                                Socks5ProxyParams.proxyCredentials(proxyParams),
                                Socks5Connect(remoteAddress)),
                              "Socks5Connection")
            context watch proxy
            context become socks5Connecting(event,
                                            proxy,
                                            connection,
                                            remoteAddress,
                                            proxyAddress)
          case None =>
            val peerAddress = peerOrProxyAddress
            logger.info(s"connected to $peerAddress")
            context watch connection
            val _ = handleEvent(event, connection, ByteVector.empty)
        }

      case metaMsg: P2PClient.MetaMsg =>
        sender() ! handleMetaMsgDisconnected(metaMsg)
    }

  def socks5Connecting(
      event: Tcp.Connected,
      proxy: ActorRef,
      connection: ActorRef,
      remoteAddress: InetSocketAddress,
      proxyAddress: InetSocketAddress): Receive = LoggingReceive {
    case Tcp.CommandFailed(_: Socks5Connect) =>
      logger.error(
        s"connection failed to $remoteAddress via SOCKS5 $proxyAddress")
      context stop self
    case Socks5Connected(_) =>
      logger.info(s"connected to $remoteAddress via SOCKS5 proxy $proxyAddress")
      context unwatch proxy
      context watch connection
      val _ = handleEvent(event, proxy, ByteVector.empty)
    case Terminated(actor) if actor == proxy =>
      context stop self
    case metaMsg: P2PClient.MetaMsg =>
      sender() ! handleMetaMsgDisconnected(metaMsg)
  }

  override def unhandled(message: Any): Unit = message match {
    case payload: LnMessage[TLV] =>
      logger.error(
        s"Cannot send a message to our peer when we are not connected! payload=$payload peer=$peer")
    case tlv: TLV =>
      logger.error(
        s"Cannot send a message to our peer when we are not connected! payload=$tlv peer=$peer")
    case _ =>
      logger.warn(s"unhandled message=$message")
  }

  /** Handles boiler plate [[Tcp.Message]] types.
    *
    * @return the unaligned bytes if we haven't received a full LnMessage yet
    */
  private def handleTcpMessage(
      message: Tcp.Message,
      peerConnection: ActorRef,
      unalignedBytes: ByteVector): ByteVector = {
    message match {
      case event: Tcp.Event =>
        handleEvent(event, peerConnection, unalignedBytes = unalignedBytes)
      case command: Tcp.Command =>
        handleCommand(command, peerConnection)

        unalignedBytes
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
        logger.debug(s"Actor is now bound to the local address: $localAddress")
        context.parent ! Tcp.Bound(localAddress)

        unalignedBytes
      case Tcp.CommandFailed(command) =>
        logger.debug(s"Client Command failed: $command")

        unalignedBytes
      case Tcp.Connected(remote, local) =>
        logger.debug(s"Tcp connection to: $remote")
        logger.debug(s"Local: $local")

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
          Tcp.PeerClosed) =>
        logger.info(s"We've been disconnected by $peer command=$closeCmd")
        //tell our peer message handler we are disconnecting
        val newPeerMsgRecv = currentPeerMsgHandlerRecv.disconnect()

        currentPeerMsgHandlerRecv = newPeerMsgRecv
        context.stop(self)
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
            LnMessage[TLV]) => Future[PeerMessageReceiver] = {
          case (peerMsgRecv: PeerMessageReceiver, m: LnMessage[TLV]) =>
            logger.trace(s"Processing message=$m")
            val msg = LnMessageReceived(m, P2PClient(self, peer))
            peerMsgRecv.handleLnMessageReceived(msg)
        }

        logger.trace(s"About to process ${messages.length} messages")
        val newMsgReceiverF =
          FutureUtil.foldLeftAsync(currentPeerMsgHandlerRecv, messages)(f)(
            context.dispatcher)

        val newMsgReceiver = Await.result(newMsgReceiverF, timeout)
        currentPeerMsgHandlerRecv = newMsgReceiver
        peerConnection ! Tcp.ResumeReading
        newUnalignedBytes
    }
  }

  /** This function is responsible for handling a [[Tcp.Command]] algebraic data type
    */
  private def handleCommand(
      command: Tcp.Command,
      peerConnection: ActorRef): Unit =
    command match {
      case closeCmd @ (Tcp.ConfirmedClose | Tcp.Close | Tcp.Abort) =>
        peerConnection ! closeCmd
      case connectCmd: Tcp.Connect =>
        manager ! connectCmd
      case bind: Tcp.Bind =>
        manager ! bind
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
  private def sendLnMessage(
      message: LnMessage[TLV],
      peerConnection: ActorRef): Unit = {

    val byteMessage = CompactByteString(message.bytes.toArray)
    peerConnection ! Tcp.Write(byteMessage)
    peerConnection ! Tcp.ResumeReading
  }

}

case class P2PClient(actor: ActorRef, peer: Peer) extends Logging {
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

object P2PClient extends Logging {

  object BindCommand

  object ConnectCommand

  /** A message hierarchy that can be sent to [[P2PClientActor P2P Client Actor]]
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

  def props(peer: Peer, peerMsgHandlerReceiver: PeerMessageReceiver): Props =
    Props(classOf[P2PClientActor], peer, peerMsgHandlerReceiver)

  def apply(
      context: ActorRefFactory,
      peer: Peer,
      peerMessageReceiver: PeerMessageReceiver): P2PClient = {
    val actorRef = context.actorOf(
      props = props(peer = peer, peerMsgHandlerReceiver = peerMessageReceiver),
      name = s"${getClass.getSimpleName}-${System.currentTimeMillis()}"
    )

    P2PClient(actorRef, peer)
  }

  /** Akka sends messages as one byte stream. There is not a 1 to 1 relationship between byte streams received and
    * protocol messages. This function parses our byte stream into individual network messages
    *
    * @param bytes the bytes that need to be parsed into individual messages
    * @return the parsed [[LnMessage[TLV]]]'s and the unaligned bytes that did not parse to a message
    */
  private[bitcoins] def parseIndividualMessages(
      bytes: ByteVector): (Vector[LnMessage[TLV]], ByteVector) = {
    @tailrec
    def loop(
        remainingBytes: ByteVector,
        accum: Vector[LnMessage[TLV]]): (Vector[LnMessage[TLV]], ByteVector) = {
      if (remainingBytes.length <= 0) {
        (accum, remainingBytes)
      } else {
        // todo figure out how to properly handle unknown messages
        Try(LnMessage.parseKnownMessage(remainingBytes)) match {
          case Failure(_) =>
            // If we can't parse the entire message, continue on until we can
            // so we properly skip it
            (accum, remainingBytes)
          case Success(message) =>
            val newRemainingBytes = remainingBytes.drop(message.byteSize)
            logger.trace(
              s"Parsed a message=${message.typeName} from bytes, continuing with remainingBytes=${newRemainingBytes.length}")
            loop(newRemainingBytes, accum :+ message)
        }
      }
    }

    loop(bytes, Vector.empty)
  }
}
