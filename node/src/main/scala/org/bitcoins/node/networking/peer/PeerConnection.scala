package org.bitcoins.node.networking.peer

import akka.actor.{ActorSystem, Cancellable}
import akka.event.Logging
import akka.io.Inet.SocketOption
import akka.io.Tcp.SO.KeepAlive
import akka.stream.scaladsl.{
  BidiFlow,
  Flow,
  Keep,
  MergeHub,
  RunnableGraph,
  Sink,
  Source,
  SourceQueueWithComplete,
  Tcp
}
import akka.stream.{Attributes, KillSwitches, UniqueKillSwitch}
import akka.util.ByteString
import akka.{Done, NotUsed}
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.number.Int32
import org.bitcoins.core.p2p._
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.NodeStreamMessage.DisconnectedPeer
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.constant.NodeConstants
import org.bitcoins.node.networking.peer.PeerConnection.ConnectionGraph
import org.bitcoins.node.util.PeerMessageSenderApi
import org.bitcoins.node.{NodeStreamMessage, P2PLogger}
import org.bitcoins.tor.Socks5Connection
import scodec.bits.ByteVector

import java.net.InetSocketAddress
import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Future, Promise}

case class PeerConnection(
    peer: Peer,
    queue: SourceQueueWithComplete[NodeStreamMessage],
    peerMessageSenderApi: PeerMessageSenderApi)(implicit
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig,
    system: ActorSystem)
    extends P2PLogger {

  import system.dispatcher

  private val socket: InetSocketAddress = {
    nodeAppConfig.socks5ProxyParams match {
      case Some(proxy) => proxy.address
      case None        => peer.socket
    }
  }

  private val options: Vector[SocketOption] = Vector(KeepAlive(true))

  private[this] var reconnectionTry = 0
  private[this] var curReconnectionTry = 0
  private[this] val reconnectionDelay = 500.millis
  private[this] var reconnectionCancellableOpt: Option[Cancellable] = None

  private def sendVersionMsg(): Future[Unit] = {
    for {
      versionMsg <- versionMsgF
      _ <- sendMsg(versionMsg)
    } yield ()
  }

  private lazy val connection: Flow[
    ByteString,
    ByteString,
    Future[Tcp.OutgoingConnection]] = {
    val base = Tcp(system).outgoingConnection(remoteAddress = socket,
                                              halfClose = false,
                                              options = options)
    nodeAppConfig.socks5ProxyParams match {
      case Some(s) =>
        base.viaMat(
          Socks5Connection.socks5Handler(peer = peer,
                                         sink = mergeHubSink,
                                         onHandshakeComplete = sendVersionMsg,
                                         credentialsOpt = s.credentialsOpt))(
          Keep.left)
      case None =>
        base
    }
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

  private def parseHelper(
      unalignedBytes: ByteString,
      byteVec: ByteString): (ByteString, Vector[NetworkMessage]) = {
    val bytes: ByteVector = ByteVector(unalignedBytes ++ byteVec)
    logger.trace(s"Bytes for message parsing: ${bytes.toHex}")
    val (messages, newUnalignedBytes) =
      NetworkUtil.parseIndividualMessages(bytes)

    if (messages.nonEmpty) updateLastParsedMessageTime()

    (ByteString.fromArray(newUnalignedBytes.toArray), messages)
  }

  private val parseToNetworkMsgFlow: Flow[
    ByteString,
    Vector[NetworkMessage],
    NotUsed] = {
    Flow[ByteString]
      .statefulMap(() => ByteString.empty)(parseHelper,
                                           { _: ByteString => None })
      .log(
        "parseToNetworkMsgFlow",
        { case msgs: Vector[NetworkMessage] =>
          s"received msgs=${msgs.map(_.payload.commandName)} from peer=$peer socket=$socket"
        })
      .withAttributes(Attributes.logLevels(onFailure = Logging.DebugLevel))
  }

  private val writeNetworkMsgFlow: Flow[ByteString, ByteString, NotUsed] = {
    Flow.apply
  }

  private val bidiFlow: BidiFlow[
    ByteString,
    Vector[NetworkMessage],
    ByteString,
    ByteString,
    NotUsed] = {
    BidiFlow.fromFlows(parseToNetworkMsgFlow, writeNetworkMsgFlow)
  }

  private val (mergeHubSink: Sink[ByteString, NotUsed],
               mergeHubSource: Source[ByteString, NotUsed]) = {
    MergeHub
      .source[ByteString](1024)
      .preMaterialize()
  }

  private val connectionFlow: Flow[
    ByteString,
    Vector[NetworkMessage],
    (Future[Tcp.OutgoingConnection], UniqueKillSwitch)] =
    connection
      .viaMat(KillSwitches.single)(Keep.both)
      .joinMat(bidiFlow)(Keep.left)

  private def connectionGraph(
      handleNetworkMsgSink: Sink[
        Vector[NetworkMessage],
        Future[Done]]): RunnableGraph[
    ((Future[Tcp.OutgoingConnection], UniqueKillSwitch), Future[Done])] = {
    val result = mergeHubSource
      .viaMat(connectionFlow)(Keep.right)
      .toMat(handleNetworkMsgSink)(Keep.both)

    result
  }

  private def buildConnectionGraph(): RunnableGraph[
    ((Future[Tcp.OutgoingConnection], UniqueKillSwitch), Future[Done])] = {

    val handleNetworkMsgSink: Sink[Vector[NetworkMessage], Future[Done]] = {
      Flow[Vector[NetworkMessage]]
        .mapConcat(identity)
        .mapAsync(1) { case msg =>
          val wrapper = msg.payload match {
            case c: ControlPayload =>
              NodeStreamMessage.ControlMessageWrapper(c, peer)
            case d: DataPayload =>
              NodeStreamMessage.DataMessageWrapper(d, peer)
          }
          queue.offer(wrapper)
        }
        .toMat(Sink.ignore)(Keep.right)
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

        val ((outgoingConnectionF: Future[Tcp.OutgoingConnection],
              killswitch: UniqueKillSwitch),
             streamDoneF) = {
          buildConnectionGraph().run()
        }

        val initializationCancellable =
          system.scheduler.scheduleOnce(nodeAppConfig.initializationTimeout) {
            val offerF =
              queue.offer(NodeStreamMessage.InitializationTimeout(peer))
            offerF.failed.foreach(err =>
              logger.error(s"Failed to offer initialize timeout for peer=$peer",
                           err))
          }

        outgoingConnectionF.onComplete {
          case scala.util.Success(o) =>
            logger.info(
              s"Connected to remote=${o.remoteAddress}  local=${o.localAddress}")
          case scala.util.Failure(err) =>
            logger.info(
              s"Failed to connect to peer=$peer with errMsg=${err.getMessage}")
        }

        val graph = ConnectionGraph(
          mergeHubSink = mergeHubSink,
          connectionF = outgoingConnectionF,
          streamDoneF = streamDoneF,
          killswitch = killswitch,
          initializationCancellable = initializationCancellable
        )

        connectionGraphOpt = Some(graph)

        val resultF: Future[Unit] = {
          for {
            _ <- outgoingConnectionF
            _ = resetReconnect()
            _ = initializationCancellable.cancel()
            versionMsg <- versionMsgF
            _ = {
              nodeAppConfig.socks5ProxyParams match {
                case Some(p) =>
                  val greetingBytes =
                    Socks5Connection.socks5Greeting(p.credentialsOpt.isDefined)
                  logger.debug(s"Writing socks5 greeting")
                  sendMsg(greetingBytes, graph.mergeHubSink)
                case None => sendMsg(versionMsg)
              }
            }
          } yield ()
        }

        val _ = graph.streamDoneF
          .onComplete {
            case scala.util.Success(_) =>
              val disconnectedPeer = DisconnectedPeer(peer, false)
              queue.offer(disconnectedPeer)
            case scala.util.Failure(err) =>
              logger.info(
                s"Connection with peer=$peer failed with err=${err.getMessage}")
              val disconnectedPeer = DisconnectedPeer(peer, false)
              queue.offer(disconnectedPeer)
          }

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
        logger.info(s"Attempting to reconnect peer=$peer")
        val delay = reconnectionDelay * (1 << curReconnectionTry)
        curReconnectionTry += 1
        reconnectionTry = reconnectionTry + 1

        val reconnectP = Promise[Unit]()
        val cancellable = system.scheduler.scheduleOnce(delay) {
          val connF = connect()
          connF.onComplete {
            case scala.util.Success(_) =>
              resetReconnect()
              reconnectP.success(())
            case scala.util.Failure(exception) =>
              logger.error(s"Failed to reconnect with peer=$peer", exception)
              reconnectP.failure(exception)
          }
        }
        reconnectionCancellableOpt = Some(cancellable)
        reconnectP.future
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
        cg.stop()
        Future.unit
      case None =>
        val err =
          s"Cannot disconnect client that is not connected to peer=${peer}!"
        logger.warn(err)
        Future.unit
    }
  }

  private[node] def sendMsg(msg: NetworkPayload): Future[Unit] = {
    //version or verack messages are the only messages that
    //can be sent before we are fully initialized
    //as they are needed to complete our handshake with our peer
    val networkMsg = NetworkMessage(nodeAppConfig.network, msg)
    sendMsg(networkMsg)
  }

  private[node] def sendMsg(msg: NetworkMessage): Future[Unit] = {
    logger.debug(
      s"Sending msg=${msg.header.commandName} to peer=${peer} socket=$socket")
    connectionGraphOpt match {
      case Some(g) =>
        sendMsg(msg.bytes, g.mergeHubSink)
      case None =>
        val exn = new RuntimeException(
          s"Could not send msg=${msg.payload.commandName} because we do not have an active connection to peer=${peer} socket=$socket")
        Future.failed(exn)
    }
  }

  private def sendMsg(
      bytes: ByteVector,
      mergeHubSink: Sink[ByteString, NotUsed]): Future[Unit] = {
    sendMsg(ByteString.fromArray(bytes.toArray), mergeHubSink)
  }

  private def sendMsg(
      bytes: ByteString,
      mergeHubSink: Sink[ByteString, NotUsed]): Future[Unit] = {
    val sendMsgF = Future {
      Source.single(bytes).to(mergeHubSink).run()
    }.map(_ => ())
    sendMsgF
  }

  private[this] val INACTIVITY_TIMEOUT: FiniteDuration =
    nodeAppConfig.inactivityTimeout

  @volatile private[this] var lastSuccessfulParsedMsgOpt: Option[Long] = None

  private def updateLastParsedMessageTime(): Unit = {
    lastSuccessfulParsedMsgOpt = Some(System.currentTimeMillis())
    ()
  }

  def isConnectionTimedOut: Boolean = {
    lastSuccessfulParsedMsgOpt match {
      case Some(lastSuccessfulParsedMsg) =>
        val timeoutInstant =
          Instant.now().minus(INACTIVITY_TIMEOUT.toMillis, ChronoUnit.MILLIS)
        val diff = Instant
          .ofEpochMilli(lastSuccessfulParsedMsg)
          .minus(timeoutInstant.toEpochMilli, ChronoUnit.MILLIS)

        val isTimedOut = diff.toEpochMilli < 0

        isTimedOut
      case None => false //we are not initialized yet
    }

  }
}

object PeerConnection {

  case class ConnectionGraph(
      mergeHubSink: Sink[ByteString, NotUsed],
      connectionF: Future[Tcp.OutgoingConnection],
      streamDoneF: Future[Done],
      killswitch: UniqueKillSwitch,
      initializationCancellable: Cancellable) {

    def stop(): Unit = {
      killswitch.shutdown()
      initializationCancellable.cancel()
      ()
    }
  }

}
