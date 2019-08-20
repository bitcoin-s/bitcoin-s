package org.bitcoins.node.networking

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.io.{IO, Tcp}
import akka.util.{ByteString, CompactByteString, Timeout}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.p2p.NetworkMessage
import org.bitcoins.core.p2p.NetworkPayload
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.PeerMessageReceiver
import org.bitcoins.node.networking.peer.PeerMessageReceiver.NetworkMessageReceived
import org.bitcoins.node.util.BitcoinSpvNodeUtil
import scodec.bits.ByteVector
import org.bitcoins.node.config.NodeAppConfig

import scala.annotation.tailrec
import scala.util._
import org.bitcoins.node.P2PLogger

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt

/**
  * This actor is responsible for creating a connection,
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
    initPeerMsgHandlerReceiver: PeerMessageReceiver
)(implicit config: NodeAppConfig)
    extends Actor
    with P2PLogger {

  private var currentPeerMsgHandlerRecv = initPeerMsgHandlerReceiver

  /**
    * The manager is an actor that handles the underlying low level I/O resources (selectors, channels)
    * and instantiates workers for specific tasks, such as listening to incoming connections.
    */
  def manager: ActorRef = IO(Tcp)(context.system)

  /**
    * The parameters for the network we are connected to
    */
  val network: NetworkParameters = config.network

  private val timeout = 10.seconds

  /**
    * TODO: this comment seems wrong?
    *
    * This actor signifies the node we are connected to on the p2p network
    * This is the context we are in after we received a [[Tcp.Connected]] message
    */
  private def awaitNetworkRequest(
      peer: ActorRef,
      unalignedBytes: ByteVector): Receive = {
    case message: NetworkMessage => sendNetworkMessage(message, peer)
    case payload: NetworkPayload =>
      val networkMsg = NetworkMessage(network, payload)
      self.forward(networkMsg)
    case message: Tcp.Message =>
      val newUnalignedBytes =
        Await.result(handleTcpMessage(message, Some(peer), unalignedBytes),
                     timeout)
      context.become(awaitNetworkRequest(peer, newUnalignedBytes))
    case metaMsg: P2PClient.MetaMsg =>
      sender ! handleMetaMsg(metaMsg)
  }

  /** This context is responsible for initializing a tcp connection with a peer on the bitcoin p2p network */
  def receive: Receive = {
    case cmd: Tcp.Command =>
      //we only accept a Tcp.Connect/Tcp.Connected
      //message to the default receive on this actor
      //after receiving Tcp.Connected we switch to the
      //'awaitNetworkRequest' context. This is the main
      //execution loop for the Client actor
      handleCommand(cmd, peerOpt = None)

    case connected: Tcp.Connected =>
      Await.result(handleEvent(connected, unalignedBytes = ByteVector.empty),
                   timeout)
    case msg: NetworkMessage =>
      self.forward(msg.payload)
    case payload: NetworkPayload =>
      logger.error(
        s"Cannot send a message to our peer when we are not connected! payload=${payload} peer=${peer}")

    case metaMsg: P2PClient.MetaMsg =>
      sender ! handleMetaMsg(metaMsg)
  }

  /**
    * Handles boiler plate [[Tcp.Message]] types.
    *
    * @return the unaligned bytes if we haven't received a full Bitcoin P2P message yet
    */
  private def handleTcpMessage(
      message: Tcp.Message,
      peer: Option[ActorRef],
      unalignedBytes: ByteVector): Future[ByteVector] = {
    message match {
      case event: Tcp.Event =>
        handleEvent(event, unalignedBytes = unalignedBytes)
      case command: Tcp.Command =>
        handleCommand(command, peer)

        Future.successful(unalignedBytes)
    }
  }

  /**
    * This function is responsible for handling a [[Tcp.Event]] algebraic data type
    */
  private def handleEvent(
      event: Tcp.Event,
      unalignedBytes: ByteVector): Future[ByteVector] = {
    import context.dispatcher
    event match {
      case Tcp.Bound(localAddress) =>
        logger.debug(
          s"Actor is now bound to the local address: ${localAddress}")
        context.parent ! Tcp.Bound(localAddress)

        Future.successful(unalignedBytes)
      case Tcp.CommandFailed(command) =>
        logger.debug(s"Client Command failed: ${command}")

        Future.successful(unalignedBytes)
      case Tcp.Connected(remote, local) =>
        logger.debug(s"Tcp connection to: ${remote}")
        logger.debug(s"Local: ${local}")

        //this is what registers a actor to send all byte messages to that is
        //received from our peer. Since we are using 'self' that means
        //our bitcoin peer will send all messages to this actor.
        sender ! Tcp.Register(self)

        val newPeerMsgRecvF: Future[PeerMessageReceiver] =
          currentPeerMsgHandlerRecv.connect(P2PClient(self, peer))
        newPeerMsgRecvF.map { newPeerMsgRecv =>
          currentPeerMsgHandlerRecv = newPeerMsgRecv
          context.become(awaitNetworkRequest(sender, unalignedBytes))
          unalignedBytes
        }

      case closeCmd @ (Tcp.ConfirmedClosed | Tcp.Closed | Tcp.Aborted |
          Tcp.PeerClosed) =>
        logger.debug(s"Closed command received: ${closeCmd}")

        //tell our peer message handler we are disconnecting
        val newPeerMsgRecvF = currentPeerMsgHandlerRecv.disconnect()

        newPeerMsgRecvF.failed.foreach(err =>
          logger.error(s"Failed to disconnect=${err}"))

        newPeerMsgRecvF.map { newPeerMsgRecv =>
          currentPeerMsgHandlerRecv = newPeerMsgRecv
          context.stop(self)
          unalignedBytes
        }

      case Tcp.Received(byteString: ByteString) =>
        val byteVec = ByteVector(byteString.toArray)
        logger.debug(s"Received ${byteVec.length} TCP bytes")
        logger.trace(s"Received TCP bytes: ${byteVec.toHex}")
        logger.trace({
          val post =
            if (unalignedBytes.isEmpty) "None"
            else unalignedBytes.toHex
          s"Unaligned bytes: $post"
        })

        //we need to aggregate our previous 'unalignedBytes' with the new message
        //we just received from our peer to hopefully be able to parse full messages
        val bytes: ByteVector = unalignedBytes ++ byteVec
        logger.trace(s"Bytes for message parsing: ${bytes.toHex}")
        val (messages, newUnalignedBytes) =
          P2PClient.parseIndividualMessages(bytes)

        logger.debug({
          val length = messages.length
          val suffix = if (length == 0) "" else s": ${messages.mkString(", ")}"

          s"Parsed $length message(s) from bytes$suffix"
        })
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
            val doneF = peerMsgRecv.handleNetworkMessageReceived(msg)
            doneF
        }

        val newMsgReceiverF: Future[PeerMessageReceiver] = {
          logger.trace(s"About to process ${messages.length} messages")
          FutureUtil.foldLeftAsync(currentPeerMsgHandlerRecv, messages)(f)
        }

        newMsgReceiverF.map { newMsgReceiver =>
          currentPeerMsgHandlerRecv = newMsgReceiver
          newUnalignedBytes
        }
    }
  }

  /**
    * This function is responsible for handling a [[Tcp.Command]] algebraic data type
    */
  private def handleCommand(
      command: Tcp.Command,
      peerOpt: Option[ActorRef]): Unit =
    command match {
      case closeCmd @ (Tcp.ConfirmedClose | Tcp.Close | Tcp.Abort) =>
        peerOpt match {
          case Some(peer) => peer ! closeCmd
          case None =>
            logger.error(
              s"Failing to disconnect node because we do not have peer defined!")
            val newPeerMsgHandlerRecvF = currentPeerMsgHandlerRecv.disconnect()
            currentPeerMsgHandlerRecv =
              Await.result(newPeerMsgHandlerRecvF, timeout)
        }
        ()
      case connectCmd: Tcp.Connect =>
        manager ! connectCmd
      case bind: Tcp.Bind =>
        manager ! bind
    }

  /**
    * Returns the current state of our peer given the [[P2PClient.MetaMsg meta message]]
    */
  private def handleMetaMsg(metaMsg: P2PClient.MetaMsg): Boolean = {
    metaMsg match {
      case P2PClient.IsConnected    => currentPeerMsgHandlerRecv.isConnected
      case P2PClient.IsInitialized  => currentPeerMsgHandlerRecv.isInitialized
      case P2PClient.IsDisconnected => currentPeerMsgHandlerRecv.isDisconnected
    }
  }

  /**
    * Sends a network request to our peer on the network
    */
  private def sendNetworkMessage(
      message: NetworkMessage,
      peer: ActorRef): Unit = {

    val byteMessage = CompactByteString(message.bytes.toArray)
    peer ! Tcp.Write(byteMessage)
  }

}

case class P2PClient(actor: ActorRef, peer: Peer) extends P2PLogger {
  import akka.pattern.ask

  def isConnected()(
      implicit timeout: Timeout,
      ec: ExecutionContext): Future[Boolean] = {
    val isConnectedF = actor.ask(P2PClient.IsConnected).mapTo[Boolean]
    isConnectedF.recoverWith {
      case _: Throwable => Future.successful(false)
    }
  }

  def isInitialized()(
      implicit timeout: Timeout,
      ec: ExecutionContext): Future[Boolean] = {
    val isInitF = actor.ask(P2PClient.IsInitialized).mapTo[Boolean]
    isInitF.recoverWith {
      case _: Throwable => Future.successful(false)
    }
  }

  def isDisconnected()(
      implicit timeout: Timeout,
      ec: ExecutionContext): Future[Boolean] = {
    val isDisconnect: Future[Boolean] =
      actor.ask(P2PClient.IsDisconnected).mapTo[Boolean]

    //this future can be failed, as we stop the P2PClientActor if we send a disconnect
    //if that actor has been killed, the peer _has_ to have been disconnected
    isDisconnect.recoverWith {
      case _: Throwable => Future.successful(true)
    }
  }
}

object P2PClient extends P2PLogger {

  /** A message hierarchy that canbe sent to [[P2PClientActor P2P Client Actor]]
    * to query about meta information of a peer
    * */
  sealed trait MetaMsg

  /** A message that can be sent to [[P2PClient p2p client]] that returns true
    * if the peer is connected, false if not */
  final case object IsConnected extends MetaMsg

  /** A message that can be sent to [[P2PClient p2p client]] that returns true
    * if the peer is initialized (p2p handshake complete), false if not */
  final case object IsInitialized extends MetaMsg

  /** A message that can be sent to [[P2PClient p2p client]] that returns true
    * if the peer is disconnected, false otherwise */
  final case object IsDisconnected extends MetaMsg

  def props(peer: Peer, peerMsgHandlerReceiver: PeerMessageReceiver)(
      implicit config: NodeAppConfig
  ): Props =
    Props(classOf[P2PClientActor], peer, peerMsgHandlerReceiver, config)

  def apply(
      context: ActorRefFactory,
      peer: Peer,
      peerMessageReceiver: PeerMessageReceiver)(
      implicit config: NodeAppConfig): P2PClient = {
    val actorRef = context.actorOf(
      props(peer = peer, peerMsgHandlerReceiver = peerMessageReceiver),
      BitcoinSpvNodeUtil.createActorName(this.getClass))

    P2PClient(actorRef, peer)
  }

  /**
    * Akka sends messages as one byte stream. There is not a 1 to 1 relationship between byte streams received and
    * bitcoin protocol messages. This function parses our byte stream into individual network messages
    *
    * @param bytes the bytes that need to be parsed into individual messages
    * @return the parsed [[NetworkMessage]]'s and the unaligned bytes that did not parse to a message
    */
  private[bitcoins] def parseIndividualMessages(bytes: ByteVector)(
      implicit conf: NodeAppConfig): (List[NetworkMessage], ByteVector) = {
    @tailrec
    def loop(
        remainingBytes: ByteVector,
        accum: List[NetworkMessage]): (List[NetworkMessage], ByteVector) = {
      if (remainingBytes.length <= 0) {
        (accum.reverse, remainingBytes)
      } else {
        val messageTry = Try(NetworkMessage(remainingBytes))
        messageTry match {
          case Success(message) =>
            if (message.header.payloadSize.toInt != message.payload.bytes.size) {
              //this means our tcp frame was not aligned, therefore put the message back in the
              //buffer and wait for the remaining bytes
              (accum.reverse, remainingBytes)
            } else {
              val newRemainingBytes = remainingBytes.slice(
                message.bytes.length,
                remainingBytes.length)
              loop(newRemainingBytes, message :: accum)
            }
          case Failure(exc) =>
            logger.error(
              s"Failed to parse network message, could be because TCP frame isn't aligned: $exc")

            //this case means that our TCP frame was not aligned with bitcoin protocol
            //return the unaligned bytes so we can apply them to the next tcp frame of bytes we receive
            //http://stackoverflow.com/a/37979529/967713
            (accum.reverse, remainingBytes)
        }
      }
    }
    val (messages, remainingBytes) = loop(bytes, Nil)
    (messages, remainingBytes)
  }

}
