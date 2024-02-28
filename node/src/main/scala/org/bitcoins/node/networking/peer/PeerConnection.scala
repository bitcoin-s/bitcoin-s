package org.bitcoins.node.networking.peer

import org.apache.pekko.{Done, NotUsed}
import org.apache.pekko.actor.{ActorSystem, Cancellable}
import org.apache.pekko.event.Logging
import org.apache.pekko.io.Inet.SocketOption
import org.apache.pekko.io.Tcp.SO.KeepAlive
import org.apache.pekko.stream.{Attributes, KillSwitches, UniqueKillSwitch}
import org.apache.pekko.stream.scaladsl.{
  BidiFlow,
  Flow,
  Keep,
  MergeHub,
  RunnableGraph,
  Sink,
  Source,
  SourceQueue,
  Tcp
}
import org.apache.pekko.util.ByteString
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
import org.bitcoins.node.{NodeStreamMessage, P2PLogger}
import org.bitcoins.tor.{Socks5Connection, Socks5ConnectionState}
import scodec.bits.ByteVector

import java.net.InetSocketAddress
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}

sealed trait PeerConnection extends P2PLogger {
  def peer: Peer
  def queue: SourceQueue[NodeStreamMessage]

  def isConnected(): Boolean

  def isDisconnected(): Boolean = {
    !isConnected()
  }
}

case class DisconnectedPeerConnection(
    peer: Peer,
    queue: SourceQueue[NodeStreamMessage])(implicit
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig,
    system: ActorSystem)
    extends PeerConnection {

  import system.dispatcher

  override def isConnected(): Boolean = false

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

  private lazy val connection: Flow[
    ByteString,
    ByteString,
    (Future[Tcp.OutgoingConnection], UniqueKillSwitch)] = {
    val base = Tcp(system)
      .outgoingConnection(remoteAddress = socket,
                          halfClose = false,
                          options = options)
      .viaMat(KillSwitches.single)(Keep.both)
    base
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

  private def sendVersionMsg(
      mergeHubSink: Sink[ByteString, NotUsed]): Future[ByteString] = {
    versionMsgF.map { versionMsg =>
      Source
        .single(ByteString(versionMsg.bytes.toArray))
        .runWith(mergeHubSink)
      ByteString.empty
    }
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
      .idleTimeout(nodeAppConfig.peerTimeout)
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

  private def buildActivePeerConnection(): Future[ActivePeerConnection] = {
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

    val runningStream: Future[ActivePeerConnection] = {
      nodeAppConfig.socks5ProxyParams match {
        case Some(s) =>
          val connectionSink =
            Flow[Either[ByteString, Socks5ConnectionState]]
              .mapAsync(1) {
                case Left(bytes) => Future.successful(bytes)
                case Right(state) =>
                  state match {
                    case Socks5ConnectionState.Connected =>
                      //need to send version message when we are first
                      //connected to initiate bitcoin protocol handshake
                      sendVersionMsg(mergeHubSink)
                    case Socks5ConnectionState.Disconnected |
                        Socks5ConnectionState.Authenticating |
                        Socks5ConnectionState.Greeted =>
                      Future.successful(ByteString.empty)
                  }
              }
              .viaMat(parseToNetworkMsgFlow)(Keep.left)
              .toMat(handleNetworkMsgSink)(Keep.right)

          val source: Source[
            ByteString,
            (Future[Tcp.OutgoingConnection], UniqueKillSwitch)] =
            mergeHubSource.viaMat(connection)(Keep.right)
          val socks5ConnF = Socks5Connection
            .socks5Handler(
              socket = peer.socket,
              source = source,
              sink = connectionSink,
              mergeHubSink = mergeHubSink,
              credentialsOpt = s.credentialsOpt
            )

          socks5ConnF.map { case ((_, killswitch), doneF) =>
            val connGraph = ConnectionGraph(
              mergeHubSink = mergeHubSink,
              connectionF = socks5ConnF.map(_._1._1),
              streamDoneF = doneF,
              killswitch = killswitch
            )
            ActivePeerConnection(peer, queue, connectionGraph = connGraph)
          }

        case None =>
          val result = connectionGraph(handleNetworkMsgSink).run()
          result._1._1.map { _ =>
            val graph = ConnectionGraph(mergeHubSink = mergeHubSink,
                                        connectionF = result._1._1,
                                        streamDoneF = result._2,
                                        killswitch = result._1._2)
            ActivePeerConnection(peer, queue, graph)
          }
      }
    }

    runningStream
  }

  /** Initiates a connection with the given peer */
  def connect(): Future[ActivePeerConnection] = {
    logger.info(s"Attempting to connect to peer=${peer}")

    val activePeerConnectionF = {
      buildActivePeerConnection()
    }
    activePeerConnectionF.flatMap(_.connectionGraph.connectionF).onComplete {
      case scala.util.Success(tcp) =>
        logger.debug(
          s"Connected to remote=${tcp.remoteAddress}  local=${tcp.localAddress}")
      case scala.util.Failure(err) =>
        logger.debug(
          s"Failed to connect to peer=$peer with errMsg=${err.getMessage}")
        val offerF =
          queue.offer(NodeStreamMessage.InitializationTimeout(peer))
        offerF.failed.foreach(err =>
          logger.error(s"Failed to offer initialize timeout for peer=$peer",
                       err))
    }

    val resultF: Future[ActivePeerConnection] = {
      for {
        apc <- activePeerConnectionF
        _ = resetReconnect()
        version <- versionMsgF
        _ <- {
          nodeAppConfig.socks5ProxyParams match {
            case Some(_) => Future.unit
            case None    => apc.sendMsg(version)
          }
        }
      } yield apc
    }

    resultF
  }

  /** resets reconnect state after connecting to a peer */
  private def resetReconnect(): Unit = {
    //cancel the job for reconnecting in case we were attempting to reconnect
    reconnectionCancellableOpt.map(_.cancel())
    reconnectionCancellableOpt = None
    reconnectionTry = 0
    curReconnectionTry = 0
  }

  def reconnect(): Future[ActivePeerConnection] = {
    logger.info(s"Attempting to reconnect peer=$peer")
    val delay = reconnectionDelay * (1 << curReconnectionTry)
    curReconnectionTry += 1
    reconnectionTry = reconnectionTry + 1

    val reconnectP = Promise[ActivePeerConnection]()
    val cancellable = system.scheduler.scheduleOnce(delay) {
      val connF = connect()
      connF.onComplete {
        case scala.util.Success(apc) =>
          resetReconnect()
          reconnectP.success(apc)
        case scala.util.Failure(exception) =>
          logger.error(s"Failed to reconnect with peer=$peer", exception)
          reconnectP.failure(exception)
      }
    }
    reconnectionCancellableOpt = Some(cancellable)
    reconnectP.future
  }
}

case class ActivePeerConnection(
    peer: Peer,
    queue: SourceQueue[NodeStreamMessage],
    connectionGraph: ConnectionGraph)(implicit
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig,
    system: ActorSystem)
    extends PeerConnection {
  import system.dispatcher

  private def handleStreamComplete(): Future[Unit] = {
    val disconnectedPeer = DisconnectedPeer(peer, false)
    queue
      .offer(disconnectedPeer)
      .map(_ => ())
  }

  locally {
    connectionGraph.streamDoneF
      .flatMap(_ => handleStreamComplete())
      .recoverWith { case err =>
        logger.error(s"ActivePeerConnection failed", err)
        handleStreamComplete()
      }
  }

  private val socket: InetSocketAddress = {
    nodeAppConfig.socks5ProxyParams match {
      case Some(proxy) => proxy.address
      case None        => peer.socket
    }
  }

  override def isConnected(): Boolean = true

  /** Disconnects the given peer */
  def disconnect(): Future[DisconnectedPeerConnection] = {
    logger.debug(s"Disconnecting peer=${peer}")
    connectionGraph.stop().map { _ =>
      val dcp = DisconnectedPeerConnection(peer, queue)
      dcp
    }
  }

  def getLocalAddress: Future[InetSocketAddress] = {
    connectionGraph.connectionF.map(_.localAddress)
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
    sendMsg(msg.bytes)
  }

  private[node] def sendMsg(bytes: ByteVector): Future[Unit] = {
    val bs = ByteString(bytes.toArray)
    sendMsg(bs)
  }

  private[node] def sendMsg(byteString: ByteString): Future[Unit] = {
    val sendMsgF = Future {
      Source.single(byteString).to(connectionGraph.mergeHubSink).run()
    }.map(_ => ())
    sendMsgF
  }
}

object PeerConnection {

  case class ConnectionGraph(
      mergeHubSink: Sink[ByteString, NotUsed],
      connectionF: Future[Tcp.OutgoingConnection],
      streamDoneF: Future[Done],
      killswitch: UniqueKillSwitch) {

    def stop(): Future[Done] = {
      killswitch.shutdown()
      streamDoneF
    }
  }

}
