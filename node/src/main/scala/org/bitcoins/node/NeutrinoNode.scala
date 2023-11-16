package org.bitcoins.node

import akka.{Done, NotUsed}
import akka.actor.{ActorSystem, Cancellable}
import akka.stream.{
  ActorAttributes,
  OverflowStrategy,
  QueueOfferResult,
  Supervision
}
import akka.stream.scaladsl.{
  Keep,
  RunnableGraph,
  Source,
  SourceQueue,
  SourceQueueWithComplete
}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.node.NodeState.DoneSyncing
import org.bitcoins.core.api.node.{NodeState, NodeType, Peer}
import org.bitcoins.core.config.{MainNet, RegTest, SigNet, TestNet3}
import org.bitcoins.core.p2p.ServiceIdentifier
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.node.config.NodeAppConfig

import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

case class NeutrinoNode(
    walletCreationTimeOpt: Option[Instant],
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    actorSystem: ActorSystem,
    paramPeers: Vector[Peer])
    extends Node
    with SourceQueue[NodeStreamMessage] {
  require(
    nodeConfig.nodeType == NodeType.NeutrinoNode,
    s"We need our Neutrino mode enabled to be able to construct a Neutrino node, got=${nodeConfig.nodeType}!")

  private val isStarted: AtomicBoolean = new AtomicBoolean(false)
  implicit override def system: ActorSystem = actorSystem

  implicit override def nodeAppConfig: NodeAppConfig = nodeConfig

  implicit override def chainAppConfig: ChainAppConfig = chainConfig

  private val dataMessageStreamSource: Source[
    NodeStreamMessage,
    SourceQueueWithComplete[NodeStreamMessage]] = {
    Source
      .queue[NodeStreamMessage](
        100 * nodeAppConfig.maxConnectedPeers,
        overflowStrategy = OverflowStrategy.backpressure,
        maxConcurrentOffers = Runtime.getRuntime.availableProcessors())
  }

  private lazy val peerFinder: PeerFinder = PeerFinder(paramPeers = paramPeers,
                                                       queue = this,
                                                       skipPeers =
                                                         () => Set.empty)

  override lazy val peerManager: PeerManager = {
    PeerManager(paramPeers = paramPeers,
                walletCreationTimeOpt = walletCreationTimeOpt,
                queue = this,
                finder = peerFinder)
  }

  private[this] var queueOpt: Option[
    SourceQueueWithComplete[NodeStreamMessage]] =
    None

  private[this] var streamDoneFOpt: Option[Future[NodeState]] = None

  private val decider: Supervision.Decider = { case err: Throwable =>
    logger.error(s"Error occurred while processing p2p pipeline stream", err)
    Supervision.Resume
  }

  private def buildDataMessageStreamGraph(
      initState: NodeState,
      source: Source[NodeStreamMessage, NotUsed]): RunnableGraph[
    Future[NodeState]] = {
    val graph = source
      .toMat(peerManager.buildP2PMessageHandlerSink(initState))(Keep.right)
      .withAttributes(ActorAttributes.supervisionStrategy(decider))
    graph
  }

  override def start(): Future[NeutrinoNode] = {
    isStarted.set(true)
    val initState =
      DoneSyncing(peers = Set.empty, waitingForDisconnection = Set.empty)
    val (queue, source) =
      dataMessageStreamSource.preMaterialize()

    queueOpt = Some(queue)
    val graph =
      buildDataMessageStreamGraph(initState = initState, source = source)
    val stateF = graph.run()
    streamDoneFOpt = Some(stateF)
    val res = for {
      node <- super.start()
      _ <- peerFinder.start()
      _ = {
        val inactivityCancellable = startInactivityChecksJob()
        inactivityCancellableOpt = Some(inactivityCancellable)
      }
    } yield {
      node.asInstanceOf[NeutrinoNode]
    }

    res.failed.foreach(logger.error("Cannot start Neutrino node", _))

    res
  }

  override def stop(): Future[NeutrinoNode] = {
    logger.info(s"Stopping NeutrinoNode")
    isStarted.set(false)
    val start = System.currentTimeMillis()
    inactivityCancellableOpt.map(_.cancel())
    for {
      _ <- peerFinder.stop()
      _ <- peerManager.stop()
      _ = queueOpt.map(_.complete())
      _ <- {
        val finishedF = streamDoneFOpt match {
          case Some(f) => f
          case None    => Future.successful(Done)
        }
        finishedF
      }
      _ = {
        //reset all variables
        streamDoneFOpt = None
        inactivityCancellableOpt = None
        queueOpt = None
      }
    } yield {
      logger.info(
        s"Node stopped! It took=${System.currentTimeMillis() - start}ms")
      this
    }
  }

  /** Starts to sync our node with our peer
    * If our local best block hash is the same as our peers
    * we will not sync, otherwise we will keep syncing
    * until our best block hashes match up
    *
    * @return
    */
  override def sync(): Future[Unit] = {
    val serviceIdentifier = ServiceIdentifier.NODE_COMPACT_FILTERS
    //wait for a peer to be available to sync from...
    //due to underlying mutability in PeerManager/PeerFinder
    //we may not have a peer available for selection immediately
    val peerAvailableF = AsyncUtil.retryUntilSatisfied(
      peerManager.randomPeerWithService(serviceIdentifier).isDefined)
    for {
      _ <- peerAvailableF
      _ <- peerManager.sync(None)
    } yield ()
  }

  /** Gets the number of compact filters in the database */
  override def getFilterCount(): Future[Int] =
    chainApiFromDb().flatMap(_.getFilterCount())

  /** Returns the block height of the given block stamp */
  override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
    chainApiFromDb().flatMap(_.getHeightByBlockStamp(blockStamp))

  override def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[FilterResponse]] =
    chainApiFromDb().flatMap(_.getFiltersBetweenHeights(startHeight, endHeight))

  private[this] val INACTIVITY_CHECK_TIMEOUT = 60.seconds

  @volatile private[this] var inactivityCancellableOpt: Option[Cancellable] =
    None

  private def inactivityChecksRunnable(): Runnable = { () =>
    val peers = peerManager.peers
    logger.info(s"Running inactivity checks for peers=${peers}")
    val resultF = if (peers.nonEmpty) {
      Future
        .traverse(peers)(peerManager.inactivityChecks)
        .map(_ => ())
    } else if (isStarted.get) {
      //stop and restart to get more peers
      stop()
        .flatMap(_.start())
        .map(_ => ())
    } else {
      start().map(_ => ())
    }

    resultF.failed.foreach(err =>
      logger.error(s"Failed to run inactivity checks for peers=${peers}", err))

    Await.result(resultF, INACTIVITY_CHECK_TIMEOUT)
  }

  private def startInactivityChecksJob(): Cancellable = {
    //the interval is set shorter for some unit test cases
    val interval = nodeAppConfig.network match {
      case MainNet | TestNet3 | SigNet => 5.minute
      case RegTest                     => nodeAppConfig.inactivityTimeout
    }
    system.scheduler.scheduleAtFixedRate(
      initialDelay = interval,
      interval = interval)(inactivityChecksRunnable())
  }

  override def offer(elem: NodeStreamMessage): Future[QueueOfferResult] = {
    queueOpt match {
      case Some(queue) => queue.offer(elem)
      case None =>
        Future.failed(new RuntimeException(
          s"NeutrinoNode not started, cannot process p2p message until NeutrinoNode.start() is called"))
    }
  }

  override def watchCompletion(): Future[Done] = {
    queueOpt match {
      case Some(queue) => queue.watchCompletion()
      case None        => Future.successful(Done)
    }
  }
}
