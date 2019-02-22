package org.bitcoins.node.networking.peer

import akka.actor.ActorRefFactory
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.db.DbConfig
import org.bitcoins.node.messages._
import org.bitcoins.node.messages.control.VersionMessage
import org.bitcoins.node.networking.Client

import scala.concurrent.{Future, Promise}

/**
  * Responsible for receiving messages from a peer on the
  * p2p network
  */
class PeerMessageReceiver(dbConfig: DbConfig)(implicit ref: ActorRefFactory)
    extends BitcoinSLogger {

  import ref.dispatcher

  /** This promise gets completed when the
    * [[connect()]] method is called.
    */
  private val clientConnectP: Promise[Client] = Promise()

  /** The [[org.bitcoins.node.networking.Client]] we are
    * connected to. This isn't initiated until the a client
    * has called the [[connect()]] method.
    */
  private val clientConnectF: Future[Client] = clientConnectP.future

  /** This promise is completed in the [[disconnect()]]
    * when a [[Client]] initiates a disconnections from
    * our peer on the p2p network
    */
  private val clientDisconnectP: Promise[Unit] = Promise()

  private val clientDisconnectF: Future[Unit] = clientDisconnectP.future

  /** If this future is completed, we are
    * connected to our client. Note, there is
    * no timeout on this future and no guarantee
    * that some one has actually initiated
    * a connection with a [[Client]]
    * @return
    */
  def isConnected(): Boolean = {
    clientConnectF.isCompleted && !clientDisconnectF.isCompleted
  }

  def isDisconnected(): Boolean = {
    clientDisconnectF.isCompleted
  }

  /** This method is called when we have received
    * a [[akka.io.Tcp.Connected]] message from our peer
    * This means we have opened a Tcp connection,
    * but have NOT started the handshake
    * This method will initiate the handshake
    */
  protected[networking] def connect(client: Client): Unit = {
    logger.debug(s"Connected with client, sending version message to peer")
    val peerMsgSender = PeerMessageSender(client)

    //complete the connection promise
    clientConnectP.success(client)

    val versionMsg = VersionMessage(Constants.networkParameters)
    val networkMsg = NetworkMessage(Constants.networkParameters, versionMsg)
    peerMsgSender.send(networkMsg)
  }

  protected[networking] def disconnect(): Unit = {
    //complete the disconnect promise, now is
    clientDisconnectP.success(())
  }

  def handleNetworkMessageReceived(
      networkMsgRecv: PeerMessageReceiver.NetworkMessageReceived): Unit = {

    //create a way to send a response if we need too
    val peerMsgSender = PeerMessageSender(networkMsgRecv.client)

    networkMsgRecv.msg.payload match {
      case controlPayload: ControlPayload =>
        handleControlPayload(payload = controlPayload, sender = peerMsgSender)
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
    val dataMsgHandler = new DataMessageHandler(dbConfig = dbConfig)
    //else it means we are receiving this data payload from a peer,
    //we need to handle it
    dataMsgHandler.handleDataPayload(payload, sender)
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
      sender: PeerMessageSender): Unit = {
    logger.debug("Control payload before derive: " + payload)
    val controlMsgHandler = new ControlMessageHandler(dbConfig)

    controlMsgHandler.handleControlPayload(payload, sender)
  }

}

object PeerMessageReceiver {
  sealed abstract class PeerMessageReceiverMsg {

    /** Who we need to use to send a reply to our peer
      * if a response is needed for this message
      * @return
      */
    def client: Client
  }

  case class NetworkMessageReceived(msg: NetworkMessage, client: Client)
      extends PeerMessageReceiverMsg

  def apply(dbConfig: DbConfig)(
      implicit ref: ActorRefFactory): PeerMessageReceiver = {
    new PeerMessageReceiver(dbConfig)(ref)
  }
}
