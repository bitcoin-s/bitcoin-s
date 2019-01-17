package org.bitcoins.node.networking.peer

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.event.LoggingReceive
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.db.DbConfig
import org.bitcoins.node.messages._
import org.bitcoins.node.networking.Client
import org.bitcoins.node.networking.peer.PeerMessageSender.HandshakeFinished
import org.bitcoins.node.util.BitcoinSpvNodeUtil

import scala.concurrent.ExecutionContext

/**
  * Responsible for receiving messages from a peer on the
  * p2p network
  */
class PeerMessageReceiverActor(dbConfig: DbConfig) extends Actor with BitcoinSLogger {
  private implicit val ec: ExecutionContext = context.dispatcher

  override def receive: Receive = LoggingReceive {
    case networkMessage: NetworkMessage =>
      //hack to get around using 'self' as the sender
      self.tell(networkMessage.payload, sender)
    case controlPayload: ControlPayload =>
      val client = Client(sender)
      handleControlPayload(controlPayload, client)
    case dataPayload: DataPayload =>
      val client = Client(sender)
      handleDataPayload(dataPayload, client)
      case HandshakeFinished =>
        logger.warn(s"HandshakeFinished should not be receved in peerMessageHandler context")
    }



  /**
    * Handles a [[DataPayload]] message. It checks if the sender is the parent
    * actor, it sends it to our peer on the network. If the sender was the
    * peer on the network, forward to the actor that spawned our actor
    *
    * @param payload
    * @param sender
    */
  private def handleDataPayload(payload: DataPayload, client: Client): Unit = {
    val dataMsgHandler = new DataMessageHandler(dbConfig = dbConfig)
    //else it means we are receiving this data payload from a peer,
    //we need to handle it
    dataMsgHandler.handleDataPayload(payload, client)
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
                                    sender: Client): Unit = {
    logger.debug("Control payload before derive: " + payload)
    val controlMsgHandler = new ControlMessageHandler(dbConfig)

    controlMsgHandler.handleControlPayload(payload, sender)
  }


}

object PeerMessageReceiver {


  def props(dbConfig: DbConfig): Props = {
    Props(classOf[PeerMessageReceiverActor], dbConfig)
  }


  def apply(dbConfig: DbConfig)(implicit ref: ActorRefFactory): ActorRef = {
    ref.actorOf(props(dbConfig), name = BitcoinSpvNodeUtil.createActorName(getClass.getSimpleName))
  }
}


case class PeerMessageReceiver(actor: ActorRef)
