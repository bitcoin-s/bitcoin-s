package org.bitcoins.node.networking.peer

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, Cancellable}
import akka.io.Tcp.SO.KeepAlive
import akka.stream.{KillSwitches, UniqueKillSwitch}
import akka.stream.scaladsl.{
  BidiFlow,
  Flow,
  Keep,
  MergeHub,
  RunnableGraph,
  Sink,
  Source,
  Tcp
}
import akka.util.ByteString
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.number.Int32
import org.bitcoins.core.p2p._
import org.bitcoins.core.util.{FutureUtil, NetworkUtil}
import org.bitcoins.crypto.{DoubleSha256Digest, HashDigest}
import org.bitcoins.node.P2PLogger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.constant.NodeConstants
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.PeerMessageReceiver.NetworkMessageReceived
import org.bitcoins.node.networking.peer.PeerMessageSender.ConnectionGraph
import org.bitcoins.node.util.PeerMessageSenderApi
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

case class PeerMessageSender(
    peer: Peer,
    initPeerMessageRecv: PeerMessageReceiver,
    peerMessageSenderApi: PeerMessageSenderApi)(implicit
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig,
    system: ActorSystem)
    extends P2PLogger {
  import system.dispatcher

  private val socket = peer.socket

  private val options = Vector(KeepAlive(true))

  private[this] var reconnectionTry = 0
  private[this] var curReconnectionTry = 0
  private[this] val reconnectionDelay = 500.millis
  private[this] var reconnectionCancellableOpt: Option[Cancellable] = None

  private lazy val connection: Flow[
    ByteString,
    ByteString,
    Future[Tcp.OutgoingConnection]] = {
    Tcp(system).outgoingConnection(peer.socket,
                                   halfClose = false,
                                   options = options)
  }

  private def parseHelper(
      unalignedBytes: ByteString,
      byteVec: ByteString): (ByteString, Vector[NetworkMessage]) = {
    val bytes: ByteVector = ByteVector(unalignedBytes ++ byteVec)
    logger.trace(s"Bytes for message parsing: ${bytes.toHex}")
    val (messages, newUnalignedBytes) =
      NetworkUtil.parseIndividualMessages(bytes)
    (ByteString.fromArray(newUnalignedBytes.toArray), messages)
  }

  private val chainApi = ChainHandler.fromDatabase()

  private val versionMsgF: Future[NetworkMessage] = {
    chainApi.getBestHashBlockHeight().map { height =>
      val localhost = java.net.InetAddress.getLocalHost
      val versionMsg =
        VersionMessage(nodeAppConfig.network,
                       NodeConstants.userAgent,
                       Int32(height),
                       InetAddress(localhost.getAddress),
                       InetAddress(localhost.getAddress),
                       nodeAppConfig.relay)
      NetworkMessage(nodeAppConfig.network, versionMsg)
    }
  }

  private val parseToNetworkMsgFlow: Flow[
    ByteString,
    Vector[NetworkMessage],
    NotUsed] = {
    Flow[ByteString]
      .statefulMap(() => ByteString.empty)(parseHelper,
                                           { _: ByteString => None })
      .log("parseToNetworkMsgFlow",
           { case msgs: Vector[NetworkMessage] =>
             msgs
               .map(msg =>
                 s"received msg=${msg.payload.commandName} from peer=$peer")
               .mkString("\n")
           })
  }

  private val writeNetworkMsgFlow: Flow[NetworkMessage, ByteString, NotUsed] = {
    Flow[NetworkMessage].map(msg => ByteString(msg.bytes.toArray))
  }

  private val bidiFlow: BidiFlow[
    ByteString,
    Vector[NetworkMessage],
    NetworkMessage,
    ByteString,
    NotUsed] = {
    BidiFlow.fromFlows(parseToNetworkMsgFlow, writeNetworkMsgFlow)
  }

  private val mergeHubSource: Source[
    NetworkMessage,
    Sink[NetworkMessage, NotUsed]] =
    MergeHub
      .source[NetworkMessage](16)
      .log("mergehub",
           { case msg: NetworkMessage =>
             s"sending msg=${msg.payload.commandName} to peer=$peer"
           })

  private val connectionFlow: Flow[
    NetworkMessage,
    Vector[NetworkMessage],
    (Future[Tcp.OutgoingConnection], UniqueKillSwitch)] =
    connection
      .viaMat(KillSwitches.single)(Keep.both)
      .joinMat(bidiFlow)(Keep.left)

  private def connectionGraph(
      handleNetworkMsgSink: Sink[
        Vector[NetworkMessage],
        Future[PeerMessageReceiver]]): RunnableGraph[
    (
        (
            Sink[NetworkMessage, NotUsed],
            (Future[Tcp.OutgoingConnection], UniqueKillSwitch)),
        Future[PeerMessageReceiver])] = {
    val result = mergeHubSource
      .viaMat(connectionFlow)(Keep.both)
      .toMat(handleNetworkMsgSink)(Keep.both)

    result
  }

  private def buildConnectionGraph(): RunnableGraph[
    (
        (
            Sink[NetworkMessage, NotUsed],
            (Future[Tcp.OutgoingConnection], UniqueKillSwitch)),
        Future[PeerMessageReceiver])] = {
    val initializing =
      initPeerMessageRecv.connect(peer, peerMessageSenderApi)

    val handleNetworkMsgSink: Sink[
      Vector[NetworkMessage],
      Future[PeerMessageReceiver]] = {

      Flow[Vector[NetworkMessage]]
        .foldAsync(initializing) { case (peerMsgRecv, msgs) =>
          FutureUtil.foldLeftAsync(peerMsgRecv, msgs) { case (p, msg) =>
            p.handleNetworkMessageReceived(networkMsgRecv =
                                             NetworkMessageReceived(msg, peer),
                                           peerMessageSenderApi =
                                             peerMessageSenderApi)
          }
        }
        .toMat(Sink.last)(Keep.right)
    }

    connectionGraph(handleNetworkMsgSink)
  }

  @volatile private[this] var connectionGraphOpt: Option[ConnectionGraph] = None

  /** Initiates a connection with the given peer */
  def connect(): Future[Unit] = {
    connectionGraphOpt match {
      case Some(_) =>
        logger.warn(s"Connected already to peer=${peer}")
        Future.unit
      case None =>
        logger.info(s"Attempting to connect to peer=${peer}")

        val ((mergeHubSink: Sink[NetworkMessage, NotUsed],
              (outgoingConnectionF: Future[Tcp.OutgoingConnection],
               killswitch: UniqueKillSwitch)),
             streamDoneF) = {
          buildConnectionGraph().run()
        }

        outgoingConnectionF.map { o =>
          logger.info(
            s"Connected to remote=${o.remoteAddress}  local=${o.localAddress}")
        }

        val graph = ConnectionGraph(mergeHubSink = mergeHubSink,
                                    connectionF = outgoingConnectionF,
                                    streamDoneF = streamDoneF,
                                    killswitch = killswitch)

        connectionGraphOpt = Some(graph)

        val resultF: Future[NotUsed] = for {
          _ <- outgoingConnectionF
          _ = resetReconnect()
          versionMsg <- versionMsgF
        } yield Source.single(versionMsg).runWith(mergeHubSink)

        val _ = graph.streamDoneF
          .flatMap { p =>
            p.disconnect(peer)
          }
          .failed
          .foreach(err =>
            logger.error(s"Failed disconnect callback with peer=$peer", err))

        resultF.map(_ => ())
    }
  }

  /** resets reconnect state after connecting to a peer */
  private def resetReconnect(): Unit = {
    //cancel the job for reconnecting in case we were attempting to reconnect
    reconnectionCancellableOpt.map(_.cancel())
    reconnectionCancellableOpt = None
    reconnectionTry = 0
    curReconnectionTry = 0
  }

  def reconnect(): Future[Unit] = {
    connectionGraphOpt match {
      case Some(_) =>
        logger.error(
          s"Cannot reconnect when we have an active connection to peer=$peer")
        Future.unit
      case None =>
        val delay = reconnectionDelay * (1 << curReconnectionTry)
        curReconnectionTry += 1
        reconnectionTry = reconnectionTry + 1

        val cancellable = system.scheduler.scheduleOnce(delay) {
          connect() //dropping future here?
          ()
        }
        reconnectionCancellableOpt = Some(cancellable)
        Future.unit
    }
  }

  def isConnected(): Future[Boolean] = {
    Future.successful(connectionGraphOpt.isDefined)
  }

  def isDisconnected(): Future[Boolean] = {
    isConnected().map(!_)
  }

  /** Disconnects the given peer */
  def disconnect(): Future[Unit] = {
    connectionGraphOpt match {
      case Some(cg) =>
        logger.info(s"Disconnecting peer=${peer}")
        cg.killswitch.shutdown()
        connectionGraphOpt = None
        Future.unit
      case None =>
        val err =
          s"Cannot disconnect client that is not connected to peer=${peer}!"
        logger.warn(err)
        Future.unit
    }
  }

  def sendFilterClearMessage(): Future[Unit] = {
    sendMsg(FilterClearMessage)
  }

  def sendFilterAddMessage(hash: HashDigest): Future[Unit] = {
    val message = FilterAddMessage.fromHash(hash)
    logger.trace(s"Sending filteradd=$message to peer=${peer}")
    sendMsg(message)
  }

  def sendFilterLoadMessage(bloom: BloomFilter): Future[Unit] = {
    val message = FilterLoadMessage(bloom)
    logger.trace(s"Sending filterload=$message to peer=${peer}")
    sendMsg(message)
  }

  def sendGetCompactFilterCheckPointMessage(
      stopHash: DoubleSha256Digest): Future[Unit] = {
    val message = GetCompactFilterCheckPointMessage(stopHash)
    logger.debug(s"Sending getcfcheckpt=$message to peer ${peer}")
    sendMsg(message)
  }

  private[node] def sendMsg(msg: NetworkPayload): Future[Unit] = {
    //version or verack messages are the only messages that
    //can be sent before we are fully initialized
    //as they are needed to complete our handshake with our peer
    val networkMsg = NetworkMessage(nodeAppConfig.network, msg)
    sendMsg(networkMsg)
  }

  private[node] def sendMsg(msg: NetworkMessage): Future[Unit] = {
    logger.debug(s"Sending msg=${msg.header.commandName} to peer=${socket}")
    connectionGraphOpt match {
      case Some(g) =>
        val sendMsgF = Future {
          Source.single(msg).to(g.mergeHubSink).run()
        }.map(_ => ())

        sendMsgF
      case None =>
        val exn = new RuntimeException(
          s"Could not send msg=${msg.payload.commandName} because we do not have an active connection to peer=${peer}")
        Future.failed(exn)
    }
  }
}

object PeerMessageSender {

  case class ConnectionGraph(
      mergeHubSink: Sink[NetworkMessage, NotUsed],
      connectionF: Future[Tcp.OutgoingConnection],
      streamDoneF: Future[PeerMessageReceiver],
      killswitch: UniqueKillSwitch)

  sealed abstract class PeerMessageHandlerMsg

  /** For when we are done with exchanging version and verack messages
    * This means we can send normal p2p messages now
    */
  case object HandshakeFinished extends PeerMessageHandlerMsg

  /** Accumulators network messages while we are doing a handshake with our peer
    * and caches a peer handler actor so we can send a [[HandshakeFinished]]
    * message back to the actor when we are fully connected
    */
  case class MessageAccumulator(
      networkMsgs: Vector[(ActorRef, NetworkMessage)],
      peerHandler: ActorRef)

}
