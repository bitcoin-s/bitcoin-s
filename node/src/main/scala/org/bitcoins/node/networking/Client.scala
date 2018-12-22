package org.bitcoins.node.networking

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}
import akka.util.ByteString
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.messages.NetworkPayload
import org.bitcoins.node.util.BitcoinSpvNodeUtil
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.messages._
import org.bitcoins.node.util.BitcoinSpvNodeUtil
import scodec.bits.ByteVector

/**
  * Created by chris on 6/6/16.
  * This actor is responsible for creating a connection,
  * relaying messages and closing a connection to our peer on
  * the p2p network. This is the actor that directly interacts
  * with the p2p network. It's responsibly is to deal with low
  * level [[Tcp.Message]].
  *
  * If the [[Client]] receives a [[NetworkMessage]], from a [[org.bitcoins.node.networking.peer.PeerMessageHandler]]
  * it serializes the message to it to a [[akka.util.ByteString]] and then sends it to the [[manager]]
  * which streams the data to our peer on the bitcoin network.
  *
  * If the [[Client]] receives a [[Tcp.Received]] message, it means we have received
  * a message from our peer on the bitcoin p2p network. This means we try to parse
  * the [[ByteString]] into a [[NetworkMessage]]. If we successfully parse the message
  * we relay that message to the [[org.bitcoins.node.networking.peer.PeerMessageHandler]]
  * that created the Client Actor.
  *
  * In this class you will see a 'unalignedBytes' value passed around in a lot of methods
  * This is because we cannot assume that a Bitcoin [[NetworkMessage]] aligns with a tcp packet.
  * For instance, a large [[BlockMessage]] (up to 4MB in size)
  * CANNOT fit in a single tcp packet. This means we must cache
  * the bytes and wait for the rest of them to be sent.
  */
sealed abstract class Client extends Actor with BitcoinSLogger {

  /**
    * The manager is an actor that handles the underlying low level I/O resources (selectors, channels)
    * and instantiates workers for specific tasks, such as listening to incoming connections.
    */
  def manager: ActorRef = IO(Tcp)(context.system)

  /**
    * The parameters for the network we are connected to
    * i.e. [[org.bitcoins.core.config.MainNet]] or [[org.bitcoins.core.config.TestNet3]]
    * @return
    */
  def network: NetworkParameters = Constants.networkParameters

  /**
    * This actor signifies the node we are connected to on the p2p network
    * This is the context we are in after we received a [[Tcp.Connected]] message
    */
  private def awaitNetworkRequest(peer: ActorRef, unalignedBytes: ByteVector): Receive = LoggingReceive {
    case message: NetworkMessage => sendNetworkMessage(message, peer)
    case payload: NetworkPayload =>
      val networkMsg = NetworkMessage(network, payload)
      self.forward(networkMsg)
    case message: Tcp.Message =>
      val newUnalignedBytes = handleTcpMessage(message, Some(peer), unalignedBytes)
      context.become(awaitNetworkRequest(peer,newUnalignedBytes))
  }

  /** This context is responsible for initializing a tcp connection with a peer on the bitcoin p2p network */
  def receive = LoggingReceive {
    case cmd: Tcp.Command =>

      //we only accept a Tcp.Connect/Tcp.Connected
      //message to the default receive on this actor
      //after receiving Tcp.Connected we switch to the
      //'awaitNetworkRequest' context. This is the main
      //execution loop for the Client actor
      val _ = handleCommand(cmd, None)


    case connected: Tcp.Connected =>

      val _ = handleEvent(connected,ByteVector.empty)
      
  }

  /**
    * Handles boiler plate [[Tcp.Message]] types.
    * @param message
    * @return the unaligned bytes if we haven't received a full bitcoin p2p message yet
    */
  private def handleTcpMessage(message: Tcp.Message, peer: Option[ActorRef], unalignedBytes: ByteVector): ByteVector = {
    message match {
      case event: Tcp.Event     =>
        handleEvent(event, unalignedBytes)
      case command: Tcp.Command =>
        handleCommand(command, peer)

        unalignedBytes
    }
  }


  /**
    * This function is responsible for handling a [[Tcp.Event]] algebraic data type
    * @param event
    */
  private def handleEvent(event: Tcp.Event, unalignedBytes: ByteVector): ByteVector = {
    event match {
      case Tcp.Bound(localAddress) =>
        logger.debug(s"Actor is now bound to the local address: ${localAddress}")
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
        sender ! Tcp.Register(self)


        context.parent ! Tcp.Connected(remote, local)
        context.become(awaitNetworkRequest(sender, ByteVector.empty))

        unalignedBytes
      case closeCmd @ (Tcp.ConfirmedClosed | Tcp.Closed | Tcp.Aborted |
                       Tcp.PeerClosed) =>
        logger.debug(s"Closed command received: ${closeCmd}")
        context.parent ! closeCmd
        context.stop(self)
        unalignedBytes
      case Tcp.Received(byteString: ByteString) =>

        //logger.debug("Received byte string in peerMessageHandler " + BitcoinSUtil.encodeHex(byteString.toArray))
        //logger.debug("Unaligned bytes: " + BitcoinSUtil.encodeHex(unalignedBytes))

        //we need to aggregate our previous 'unalignedBytes' with the new message
        //we just received from our peer to hopefully be able to parse full messages
        val bytes: ByteVector = unalignedBytes ++ ByteVector(byteString.toArray)
        //logger.debug("Bytes for message parsing: " + BitcoinSUtil.encodeHex(bytes))
        val (messages, newUnalignedBytes) =
          BitcoinSpvNodeUtil.parseIndividualMessages(bytes)

        //for the messages we successfully parsed above
        //send them to 'context.parent' -- this is the
        //PeerMessageHandler that is responsible for
        //creating this Client Actor
        messages.foreach(m => context.parent ! m)

        newUnalignedBytes
    }
  }

  /**
    * This function is responsible for handling a [[Tcp.Command]] algebraic data type
    * @param command
    */
  private def handleCommand(command: Tcp.Command, peer: Option[ActorRef]): Unit =
    command match {
      case closeCmd @ (Tcp.ConfirmedClose | Tcp.Close | Tcp.Abort) =>
        peer.map(p => p ! closeCmd)
        ()
      case connectCmd: Tcp.Connect =>
        manager ! connectCmd
      case bind: Tcp.Bind =>
        manager ! bind
    }

  /**
    * Sends a network request to our peer on the network
    * @param message
    * @return
    */
  private def sendNetworkMessage(message: NetworkMessage, peer: ActorRef): Unit = {
    val byteMessage = BitcoinSpvNodeUtil.buildByteString(message.bytes)
    logger.debug(s"Network message: ${message}")
    peer ! Tcp.Write(byteMessage)
  }

}

object Client {
  private case class ClientImpl() extends Client

  def props: Props = Props(classOf[ClientImpl])

  def apply(context: ActorContext): ActorRef =
    context.actorOf(props, BitcoinSpvNodeUtil.createActorName(this.getClass))
}
