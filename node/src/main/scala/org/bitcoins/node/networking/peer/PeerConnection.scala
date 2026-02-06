package org.bitcoins.node.networking.peer

import org.apache.pekko.actor.{ActorSystem, Cancellable}
import org.apache.pekko.event.Logging
import org.apache.pekko.io.Inet.SocketOption
import org.apache.pekko.io.Tcp.SO.KeepAlive
import org.apache.pekko.stream.*
import org.apache.pekko.stream.scaladsl.{
  BidiFlow,
  Flow,
  Keep,
  RunnableGraph,
  Sink,
  Source,
  SourceQueue,
  Tcp
}
import org.apache.pekko.util.ByteString
import org.apache.pekko.{Done, NotUsed}
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.api.node.constant.NodeConstants
import org.bitcoins.core.number.Int32
import org.bitcoins.core.p2p.*
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.NodeStreamMessage
import org.bitcoins.node.NodeStreamMessage.DisconnectedPeer
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.networking.peer.PeerConnection.ConnectionGraph
import org.bitcoins.tor.{Socks5Connection, Socks5ConnectionState}
import org.slf4j.{Logger, LoggerFactory}
import scodec.bits.ByteVector

import java.net.InetSocketAddress
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}

case class PeerConnection(
    peer: Peer,
    inboundQueue: SourceQueue[NodeStreamMessage])(implicit
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig,
    system: ActorSystem
) /*extends P2PLogger */ {
  private def logger: Logger = LoggerFactory.getLogger(getClass)
  import system.dispatcher

  private val socket: InetSocketAddress = {
    nodeAppConfig.socks5ProxyParams match {
      case Some(proxy) => proxy.address
      case None        => peer.socket
    }
  }

  private val options: Vector[SocketOption] = Vector(KeepAlive(true))

  private var reconnectionTry = 0
  private var curReconnectionTry = 0
  private val reconnectionDelay = 500.millis
  private var reconnectionCancellableOpt: Option[Cancellable] = None

  private lazy val connection: Flow[
    ByteString,
    ByteString,
    Future[Tcp.OutgoingConnection]
  ] = {
    val base = Tcp(system)
      .outgoingConnection(
        remoteAddress = socket,
        halfClose = false,
        options = options
      )
    base
  }

  private val chainApi = ChainHandler.fromDatabase()

  private val versionMsgF: Future[NetworkMessage] = {
    chainApi.getBestHashBlockHeight().map { height =>
      val localhost = java.net.InetAddress.getLocalHost
      val versionMsg =
        VersionMessage(
          nodeAppConfig.network,
          NodeConstants.userAgent,
          Int32(height),
          InetAddress(localhost.getAddress),
          InetAddress(localhost.getAddress),
          nodeAppConfig.relay
        )
      NetworkMessage(nodeAppConfig.network, versionMsg)
    }
  }

  private def sendVersionMsg(): Future[QueueOfferResult] = {
    versionMsgF.flatMap { v =>
      logger.debug(s"Sending version message=$v to peer=$peer")
      sendMsg(v.bytes, outboundQueue)
    }
  }

  private val (outboundQueue: SourceQueue[ByteString],
               outboundQueueSource: Source[ByteString, NotUsed]) =
    Source
      .queue[ByteString](
        bufferSize = NodeConstants.bufferSize,
        overflowStrategy = OverflowStrategy.backpressure
      )
      .preMaterialize()

  private val connectionFlow: Flow[ByteString,
                                   Vector[
                                     NetworkMessage
                                   ],
                                   Future[Tcp.OutgoingConnection]] =
    connection
      .idleTimeout(nodeAppConfig.peerTimeout)
      .joinMat(PeerConnection.bidiFlow)(Keep.left)

  private def connectionGraph(
      handleNetworkMsgSink: Sink[Vector[NetworkMessage],
                                 (UniqueKillSwitch, Future[Done])]
  ): RunnableGraph[
    ((Future[Tcp.OutgoingConnection], UniqueKillSwitch), Future[Done])
  ] = {

    val result = outboundQueueSource
      .viaMat(connectionFlow)(Keep.right)
      .toMat(handleNetworkMsgSink)(Keep.both)

    result.mapMaterializedValue(r => ((r._1, r._2._1), r._2._2))
  }

  private val decider: Supervision.Decider = { case err: Throwable =>
    logger.error(s"Peer connection failed with err", err)
    Supervision.Resume
  }

  private def buildConnectionGraph()
      : Future[((Tcp.OutgoingConnection, UniqueKillSwitch), Future[Done])] = {

    val handleNetworkMsgSink
        : Sink[Vector[NetworkMessage], (UniqueKillSwitch, Future[Done])] = {
      Flow[Vector[NetworkMessage]]
        .mapConcat(identity)
        .mapAsync(1) { case msg =>
          val wrapper = msg.payload match {
            case c: ControlPayload =>
              NodeStreamMessage.ControlMessageWrapper(c, peer)
            case d: DataPayload =>
              NodeStreamMessage.DataMessageWrapper(d, peer)
          }
          inboundQueue.offer(wrapper)
        }
        .withAttributes(ActorAttributes.supervisionStrategy(decider))
        .viaMat(KillSwitches.single)(Keep.right)
        .toMat(Sink.ignore)(Keep.both)
    }

    val runningStream
        : Future[((Tcp.OutgoingConnection, UniqueKillSwitch), Future[Done])] = {
      nodeAppConfig.socks5ProxyParams match {
        case Some(s) =>
          val connectionSink =
            Flow[Either[ByteString, Socks5ConnectionState]]
              .mapAsync(1) {
                case Left(bytes) => Future.successful(bytes)
                case Right(state) =>
                  state match {
                    case Socks5ConnectionState.Connected =>
                      // need to send version message when we are first
                      // connected to initiate bitcoin protocol handshake
                      sendVersionMsg().map(_ => ByteString.empty)
                    case Socks5ConnectionState.Disconnected |
                        Socks5ConnectionState.Authenticating |
                        Socks5ConnectionState.Greeted =>
                      Future.successful(ByteString.empty)
                  }
              }
              .viaMat(PeerConnection.parseToNetworkMsgFlow)(Keep.left)
              .toMat(handleNetworkMsgSink)(Keep.right)

          val source: Source[
            ByteString,
            Future[Tcp.OutgoingConnection]
          ] =
            outboundQueueSource.viaMat(connection)(Keep.right)
          Socks5Connection
            .socks5Handler(
              socket = peer.socket,
              source = source,
              sink = connectionSink,
              outboundQueue = outboundQueue,
              credentialsOpt = s.credentialsOpt
            )
            .map(r => ((r._1, r._2._1), r._2._2))

        case None =>
          val result = connectionGraph(handleNetworkMsgSink).run()
          result._1._1.map(conn => ((conn, result._1._2), result._2))
      }
    }

    runningStream
  }

  def getLocalAddress: Future[Option[InetSocketAddress]] = {
    connectionGraphOpt match {
      case Some(g) => g.connectionF.map(c => Some(c.localAddress))
      case None    => Future.successful(None)
    }
  }

  @volatile private var connectionGraphOpt: Option[ConnectionGraph] = None

  /** Initiates a connection with the given peer */
  def connect(): Future[Unit] = {
    connectionGraphOpt match {
      case Some(_) =>
        logger.warn(s"Connected already to peer=${peer}")
        Future.unit
      case None =>
        logger.debug(s"Attempting to connect to peer=${peer}")

        val outgoingConnectionF = {
          buildConnectionGraph()
        }

        outgoingConnectionF.onComplete {
          case scala.util.Success(o) =>
            val tcp = o._1._1
            logger.debug(
              s"Connected to remote=${tcp.remoteAddress}  local=${tcp.localAddress}"
            )
          case scala.util.Failure(err) =>
            logger.debug(
              s"Failed to connect to peer=$peer with errMsg=${err.getMessage}"
            )
            val offerF =
              inboundQueue.offer(NodeStreamMessage.InitializationTimeout(peer))
            offerF.failed.foreach(err =>
              logger.error(
                s"Failed to offer initialize timeout for peer=$peer",
                err
              ))
        }

        val resultF: Future[Unit] = {
          for {
            outgoingConnection <- outgoingConnectionF
            graph = ConnectionGraph(
              outboundQueue = outboundQueue,
              connectionF = outgoingConnectionF.map(_._1._1),
              streamDoneF = outgoingConnection._2,
              killswitch = outgoingConnection._1._2
            )
            _ = {
              connectionGraphOpt = Some(graph)
              val _ = graph.streamDoneF
                .onComplete {
                  case scala.util.Success(_) =>
                    handleStreamComplete()
                  case scala.util.Failure(err) =>
                    logger.debug(
                      s"Connection with peer=$peer failed with err=${err.getMessage}"
                    )
                    handleStreamComplete()
                }
            }
            _ = resetReconnect()
            _ <- {
              nodeAppConfig.socks5ProxyParams match {
                case Some(_) => Future.unit
                case None    => sendVersionMsg()
              }
            }
          } yield ()
        }
        resultF.map(_ => ())
    }
  }

  private def handleStreamComplete(): Future[Unit] = {
    val disconnectedPeer = DisconnectedPeer(peer, false)
    val offerF = inboundQueue
      .offer(disconnectedPeer)
      .map(_ => ())
    offerF
  }

  /** resets reconnect state after connecting to a peer */
  private def resetReconnect(): Unit = {
    // cancel the job for reconnecting in case we were attempting to reconnect
    reconnectionCancellableOpt.map(_.cancel())
    reconnectionCancellableOpt = None
    reconnectionTry = 0
    curReconnectionTry = 0
  }

  def reconnect(): Future[Unit] = {
    connectionGraphOpt match {
      case Some(_) =>
        logger.error(
          s"Cannot reconnect when we have an active connection to peer=$peer"
        )
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
  def disconnect(): Future[Done] = {
    connectionGraphOpt match {
      case Some(cg) =>
        logger.debug(s"Disconnecting peer=${peer}")
        connectionGraphOpt = None
        cg.stop()
      case None =>
        Future.successful(Done)
    }
  }

  private[node] def sendMsg(msg: NetworkPayload): Future[QueueOfferResult] = {
    // version or verack messages are the only messages that
    // can be sent before we are fully initialized
    // as they are needed to complete our handshake with our peer
    val networkMsg = NetworkMessage(nodeAppConfig.network, msg)
    sendMsg(networkMsg)
  }

  private[node] def sendMsg(msg: NetworkMessage): Future[QueueOfferResult] = {

    connectionGraphOpt match {
      case Some(g) =>
        sendMsg(msg.bytes, g.outboundQueue).map { offerResult =>
          logger.debug(
            s"Sending msg=${msg.header.commandName} to peer=${peer} socket=$socket offerResult=$offerResult"
          )
          offerResult
        }
      case None =>
        val log =
          s"Could not send msg=${msg.payload.commandName} because we do not have an active connection to peer=${peer} socket=$socket"
        logger.warn(log)
        Future.successful(QueueOfferResult.dropped)
    }
  }

  private def sendMsg(
      bytes: ByteVector,
      queue: SourceQueue[ByteString]
  ): Future[QueueOfferResult] = {
    sendMsg(ByteString.fromArray(bytes.toArray), queue)
  }

  private def sendMsg(
      bytes: ByteString,
      queue: SourceQueue[ByteString]
  ): Future[QueueOfferResult] = {
    queue.offer(bytes)
  }
}

object PeerConnection extends BitcoinSLogger {
  val outboundBufferSize: Int = 1024
  case class ConnectionGraph(
      outboundQueue: SourceQueue[ByteString],
      connectionF: Future[Tcp.OutgoingConnection],
      streamDoneF: Future[Done],
      killswitch: UniqueKillSwitch
  ) {

    def stop(): Future[Done] = {
      killswitch.shutdown()
      streamDoneF
    }
  }

  private def parseHelper(
      unalignedBytes: ByteString,
      byteVec: ByteString
  ): (ByteString, Vector[NetworkMessage]) = {
    val bytes: ByteVector = ByteVector(unalignedBytes ++ byteVec)
    logger.trace(s"Bytes for message parsing: ${bytes.toHex}")
    val (messages, newUnalignedBytes) =
      NetworkUtil.parseIndividualMessages(bytes)

    (ByteString.fromArray(newUnalignedBytes.toArray), messages)
  }

  val parseToNetworkMsgFlow
      : Flow[ByteString, Vector[NetworkMessage], NotUsed] = {
    Flow[ByteString]
      .statefulMap(() => ByteString.empty)(
        parseHelper,
        { (_: ByteString) => None }
      )
      .log(
        "parseToNetworkMsgFlow",
        { case msgs: Vector[NetworkMessage] =>
          s"received msgs=${msgs.map(_.payload.commandName)} from peer"
        }
      )
      .withAttributes(Attributes.logLevels(onFailure = Logging.DebugLevel))
  }

  val writeNetworkMsgFlow: Flow[ByteString, ByteString, NotUsed] = {
    Flow.apply
  }

  val bidiFlow: BidiFlow[ByteString,
                         Vector[
                           NetworkMessage
                         ],
                         ByteString,
                         ByteString,
                         NotUsed] = {
    BidiFlow.fromFlows(parseToNetworkMsgFlow, writeNetworkMsgFlow)
  }
}
