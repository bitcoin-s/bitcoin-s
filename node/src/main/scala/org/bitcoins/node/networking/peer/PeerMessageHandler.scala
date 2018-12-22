package org.bitcoins.node.networking.peer

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.event.LoggingReceive
import akka.io.Tcp
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.db.DbConfig
import org.bitcoins.node.messages._
import org.bitcoins.node.messages.control.{PongMessage, VersionMessage}
import org.bitcoins.node.networking.Client
import org.bitcoins.node.networking.peer.PeerMessageHandler.{HandshakeFinished, MessageAccumulator, PeerMessageHandlerMsg}
import org.bitcoins.node.util.BitcoinSpvNodeUtil
import scodec.bits.ByteVector

import scala.concurrent.ExecutionContext

/**
  * Created by chris on 6/7/16.
  * This actor is the middle man between our [[Client]] and higher level actors such as
  * [[org.bitcoins.node.networking.BlockActor]]. When it receives a message, it tells [[Client]] to create connection to a peer,
  * then it exchanges [[VersionMessage]], [[VerAckMessage]] and [[org.bitcoins.node.messages.PingMessage]]/[[PongMessage]] message
  * with our peer on the network. When the Client finally responds to the [[NetworkMessage]] we originally
  * sent it sends that [[NetworkMessage]] back to the actor that requested it.
  */
sealed abstract class PeerMessageHandler(dbConfig: DbConfig) extends Actor with BitcoinSLogger {
  private implicit val ec: ExecutionContext = context.dispatcher
  lazy val peer: ActorRef = context.actorOf(
    Client.props,
    BitcoinSpvNodeUtil.createActorName(this.getClass))

  def receive = LoggingReceive {
    case connect: Tcp.Connect =>
      val msgAccum = MessageAccumulator(Vector.empty, sender)
      context.become(awaitConnected(msgAccum, ByteVector.empty))
      peer ! connect
  }

  /** Waits for us to receive a [[Tcp.Connected]] message from our [[Client]] */
  def awaitConnected(
                      msgAccum: MessageAccumulator,
                      unalignedBytes: ByteVector): Receive = LoggingReceive {
    case Tcp.Connected(_, local) =>
      val versionMsg =
        VersionMessage(Constants.networkParameters, local.getAddress)
      peer ! versionMsg
      logger.info("Switching to awaitVersionMessage from awaitConnected")
      context.become(awaitVersionMessage(msgAccum, unalignedBytes))
    case msg: NetworkMessage =>
      logger.debug(
        "Received another peer request while waiting for Tcp.Connected: " + msg)

      val newNetworkMsg = (sender, msg) +: msgAccum.networkMsgs
      val newMsgAccum = MessageAccumulator(newNetworkMsg, msgAccum.peerHandler)
      context.become(awaitConnected(newMsgAccum, unalignedBytes))
    case payload: NetworkPayload =>
      self ! NetworkMessage(Constants.networkParameters, payload)
  }

  /** Waits for a peer on the network to send us a [[VersionMessage]] */
  private def awaitVersionMessage(
                                   msgAccum: MessageAccumulator,
                                   unalignedBytes: ByteVector): Receive = LoggingReceive {
    case networkMessage: NetworkMessage =>
      networkMessage.payload match {
        case _: VersionMessage =>
          peer ! VerAckMessage
          //need to wait for the peer to send back a verack message
          logger.debug("Switching to awaitVerack from awaitVersionMessage")
          context.become(awaitVerack(msgAccum, unalignedBytes))
        case _: NetworkPayload =>

          val newNetworkMsg = (sender, networkMessage) +: msgAccum.networkMsgs
          val newAccum = MessageAccumulator(newNetworkMsg, msgAccum.peerHandler)

          context.become(
            awaitVersionMessage(newAccum,
              unalignedBytes))
      }
    case payload: NetworkPayload =>
      self ! NetworkMessage(Constants.networkParameters, payload)
  }

  /** Waits for our peer on the network to send us a [[VerAckMessage]] */
  private def awaitVerack(
                           msgAccum: MessageAccumulator,
                           unalignedBytes: ByteVector): Receive = LoggingReceive {
    case networkMessage: NetworkMessage =>
      networkMessage.payload match {
        case VerAckMessage =>
          logger.info(
            "Received verack message, sending queued messages: " + msgAccum.networkMsgs)
          sendPeerRequests(msgAccum.networkMsgs)
          logger.info("Switching to peerMessageHandler from awaitVerack")
          val controlPayloads = findControlPayloads(msgAccum.networkMsgs)
          context.become(peerMessageHandler(controlPayloads, unalignedBytes))


          msgAccum.peerHandler ! HandshakeFinished
        case _: NetworkPayload =>

          val newNetworkMsg = (sender, networkMessage) +: msgAccum.networkMsgs
          val newMsgAccum = MessageAccumulator(newNetworkMsg, msgAccum.peerHandler)
          context.become(
            awaitVerack(newMsgAccum, unalignedBytes))
      }
    case payload: NetworkPayload =>
      self ! NetworkMessage(Constants.networkParameters, payload)
  }

  /**
    * Sends all of the given [[NetworkMessage]] to our peer on the p2p network
    *
    * @param requests
    * @return
    */
  private def sendPeerRequests(requests: Seq[(ActorRef, NetworkMessage)]) =
    for {
      (sender, peerRequest) <- requests
    } yield peer ! peerRequest

  /**
    * This is the main receive function inside of [[PeerMessageHandler]]
    * This will receive peer requests, then send the payload to the the corresponding
    * actor responsible for handling that specific message
    *
    * @return
    */
  def peerMessageHandler(
                          controlMessages: Seq[(ActorRef, ControlPayload)],
                          unalignedBytes: ByteVector): Receive = LoggingReceive {
    case networkMessage: NetworkMessage =>
      //hack to get around using 'self' as the sender
      self.tell(networkMessage.payload, sender)
    case controlPayload: ControlPayload =>
      val newControlMsgs =
        handleControlPayload(controlPayload, sender, controlMessages)
      context.become(peerMessageHandler(newControlMsgs, unalignedBytes))
    case dataPayload: DataPayload =>
      handleDataPayload(dataPayload, sender)
    case msg: PeerMessageHandlerMsg => msg match {
      case PeerMessageHandler.SendToPeer(msg) =>
        peer ! msg

      case HandshakeFinished =>
        logger.warn(s"HandshakeFinished should not be receved in peerMessageHandler context")
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
  private def handleDataPayload(payload: DataPayload, sender: ActorRef): Unit = {
    val dataMsgHandler = new DataMessageHandler(dbConfig = dbConfig)
    if (sender == context.parent) {
      //means we need to send this message to our peer
      val msg = NetworkMessage(Constants.networkParameters, payload)
      peer ! msg
    } else {
      //else it means we are receiving this data payload from a peer,
      //we need to handle it
      dataMsgHandler.handleDataPayload(payload)
    }
  }

  /**
    * Handles control payloads defined here https://bitcoin.org/en/developer-reference#control-messages
    *
    * @param payload  the payload we need to do something with
    * @param requests the @payload may be a response to a request inside this sequence
    * @return the requests with the request removed for which the @payload is responding too
    */
  private def handleControlPayload(
                                    payload: ControlPayload,
                                    sender: ActorRef,
                                    requests: Seq[(ActorRef, ControlPayload)]): Seq[
    (ActorRef, ControlPayload)] = {
    logger.debug("Control payload before derive: " + payload)
    val destination = deriveDestination(sender)
    payload match {
      case pingMsg: PingMessage =>
        if (destination == context.parent) {
          //means that our peer sent us a ping message, we respond with a pong
          peer ! PongMessage(pingMsg.nonce)
          //remove ping message from requests
          requests.filterNot {
            case (_, msg) => msg.isInstanceOf[PingMessage]
          }
        } else {
          //means we initialized the ping message, send it to our peer
          logger.debug("Sending ping message to peer: " + pingMsg)
          peer ! pingMsg
          requests
        }
      case SendHeadersMessage =>
        requests
      case addrMessage: AddrMessage =>
        //figure out if this was a solicited AddrMessage or an Unsolicited AddrMessage
        //see https://bitcoin.org/en/developer-reference#addr
        val getAddrMessage: Option[(ActorRef, ControlPayload)] = requests.find {
          case (_, msg) => msg == GetAddrMessage
        }
        if (getAddrMessage.isDefined) {
          destination ! addrMessage
          //remove the GetAddrMessage request
          requests.filterNot { case (_, msg) => msg == GetAddrMessage }
        } else requests
      case filterMsg@(_: FilterAddMessage | _: FilterLoadMessage |
                      FilterClearMessage) =>
        logger.debug(
          "Sending filter message: " + filterMsg + " to " + "destination")
        destination ! filterMsg
        requests
      case controlMsg@(GetAddrMessage | VerAckMessage | _: VersionMessage |
                       _: PongMessage) =>
        destination ! controlMsg
        requests
      case rejectMsg: RejectMessage =>
        logger.error(
          "Received a reject message from the p2p network: " + rejectMsg)
        requests

      case _: FeeFilterMessage =>
        requests
    }
  }

  /**
    * Figures out the actor that is the destination for a message
    * For messages, if the sender was context.parent, we need to send the message to our peer on the network
    * if the sender was the peer, we need to relay the message to the context.parent
    *
    * @param sender
    * @return
    */
  private def deriveDestination(sender: ActorRef): ActorRef = {
    if (sender == context.parent) {
      logger.debug(
        "The sender was context.parent, therefore we are sending this message to our peer on the network")
      peer
    } else {
      logger.debug(
        "The sender was the peer on the network, therefore we need to send to context.parent")
      context.parent
    }
  }

  /**
    * Finds all control payloads inside of a given sequence of requests
    *
    * @param requests
    * @return
    */
  private def findControlPayloads(
                                   requests: Seq[(ActorRef, NetworkMessage)]): Seq[
    (ActorRef, ControlPayload)] = {
    val controlPayloads = requests.filter {
      case (_, msg) => msg.payload.isInstanceOf[ControlPayload]
    }
    controlPayloads.map {
      case (sender, msg) => (sender, msg.payload.asInstanceOf[ControlPayload])
    }
  }
}

object PeerMessageHandler {

  private case class PeerMessageHandlerImpl(dbConfig: DbConfig)
    extends PeerMessageHandler(dbConfig)

  sealed abstract class PeerMessageHandlerMsg

  /**
    * For when we are done with exchanging version and verack messages
    * This means we can send normal p2p messages now
    */
  case object HandshakeFinished extends PeerMessageHandlerMsg


  case class SendToPeer(msg: NetworkMessage) extends PeerMessageHandlerMsg

  /** Accumulators network messages while we are doing a handshake with our peer
    * and caches a peer handler actor so we can send a [[HandshakeFinished]]
    * message back to the actor when we are fully connected
    *
    * @param networkMsgs
    * @param peerHandler
    */
  case class MessageAccumulator(networkMsgs: Vector[(ActorRef, NetworkMessage)],
                                peerHandler: ActorRef)

  def props: Props = {
    props(Constants.dbConfig)
  }

  def props(dbConfig: DbConfig): Props =
    Props(classOf[PeerMessageHandlerImpl], dbConfig)

  def apply(dbConfig: DbConfig)(implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props = props(dbConfig),
      name = BitcoinSpvNodeUtil.createActorName(this.getClass))

}
