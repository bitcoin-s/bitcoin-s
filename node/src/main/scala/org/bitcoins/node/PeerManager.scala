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
  Sink,
  Source,
  SourceQueue,
  SourceQueueWithComplete
}
import grizzled.slf4j.Logging
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.{ChainApi}
import org.bitcoins.core.api.chain.db.{
  BlockHeaderDb,
  CompactFilterDb,
  CompactFilterHeaderDb
}
import org.bitcoins.core.api.node.{
  NodeState,
  NodeType,
  Peer,
  PeerManagerApi,
  SyncNodeState
}
import org.bitcoins.core.p2p._
import org.bitcoins.core.util.{NetworkUtil, StartStopAsync}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{PeerDAO, PeerDb}
import org.bitcoins.core.api.node.NodeState._
import org.bitcoins.node.networking.peer._
import org.bitcoins.node.util.PeerMessageSenderApi
import NodeStreamMessage._
import org.bitcoins.core.config.{MainNet, RegTest, SigNet, TestNet3}
import scodec.bits.ByteVector

import java.net.InetAddress
import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

case class PeerManager(
    paramPeers: Vector[Peer] = Vector.empty,
    walletCreationTimeOpt: Option[Instant])(implicit
    ec: ExecutionContext,
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig)
    extends StartStopAsync[PeerManager]
    with PeerManagerApi
    with SourceQueue[NodeStreamMessage]
    with P2PLogger {
  private val isStarted: AtomicBoolean = new AtomicBoolean(false)

  private val _peerDataMap: mutable.Map[Peer, PersistentPeerData] =
    mutable.Map.empty

  private[this] var finderOpt: Option[PeerFinder] = {
    None
  }

  def addPeerToTry(peers: Vector[Peer], priority: Int = 0): Unit = {
    finderOpt match {
      case Some(finder) => finder.addToTry(peers, priority)
      case None =>
        sys.error(
          s"Cannot addPeerToTry, finder not started. Call PeerManager.start()")
    }
  }

  def connectedPeerCount: Int = _peerDataMap.size

  override def connectPeer(peer: Peer): Future[Unit] = {
    finderOpt match {
      case Some(finder) =>
        require(finder.hasPeer(peer), s"Unknown $peer marked as usable")
        val curPeerData = finder.popFromCache(peer).get
        _peerDataMap.put(peer, curPeerData)
        val hasCf =
          if (curPeerData.serviceIdentifier.nodeCompactFilters) "with filters"
          else ""
        logger.info(
          s"Connected to peer $peer $hasCf. Connected peer count $connectedPeerCount")
        Future.unit
      case None =>
        val exn = new RuntimeException(
          s"Cannot connectPeer, finder not started. Call PeerManager.start()")
        Future.failed(exn)
    }
  }

  override def peers: Set[Peer] = _peerDataMap.keys.toSet

  /** Starts sync compact filter headers.
    * Only starts syncing compact filters if our compact filter headers are in sync with block headers
    */
  private def syncCompactFilters(
      bestFilterHeader: CompactFilterHeaderDb,
      chainApi: ChainApi,
      compactFilterStartHeight: Int,
      nodeState: SyncNodeState)(implicit
      chainAppConfig: ChainAppConfig): Future[Unit] = {
    val syncPeer = nodeState.syncPeer
    val peerMsgSender = getPeerMsgSender(syncPeer) match {
      case Some(p) => p
      case None =>
        sys.error(s"Could not find peer=$syncPeer")
    }
    val sendCompactFilterHeaderMsgF =
      PeerManager.sendNextGetCompactFilterHeadersCommand(
        peerMessageSenderApi = peerMsgSender,
        chainApi = chainApi,
        peer = syncPeer,
        filterHeaderBatchSize = chainAppConfig.filterHeaderBatchSize,
        prevStopHash = bestFilterHeader.blockHashBE
      )
    sendCompactFilterHeaderMsgF.flatMap { isSyncFilterHeaders =>
      // If we have started syncing filters
      if (
        !isSyncFilterHeaders && compactFilterStartHeight < bestFilterHeader.height
      ) {
        PeerManager
          .sendNextGetCompactFilterCommand(
            peerMessageSenderApi = peerMsgSender,
            chainApi = chainApi,
            filterBatchSize = chainAppConfig.filterBatchSize,
            startHeight = compactFilterStartHeight,
            peer = syncPeer
          )
          .map(_ => ())
      } else {
        Future.unit
      }
    }
  }

  private def getPeerConnection(peer: Peer): Option[PeerConnection] = {
    _peerDataMap.find(_._1 == peer).map(_._2.peerConnection) match {
      case Some(peerConnection) => Some(peerConnection)
      case None                 => None
    }
  }

  private def randomPeerConnection(
      services: ServiceIdentifier): Option[PeerConnection] = {
    val peerOpt = randomPeerWithService(services)
    val peerConnectionOpt = peerOpt.flatMap(getPeerConnection(_))
    peerConnectionOpt
  }

  private def getPeerMsgSender(peer: Peer): Option[PeerMessageSender] = {
    val randomPeerOpt = getPeerConnection(peer)
    randomPeerOpt.map(PeerMessageSender(_))
  }

  def randomPeerWithService(services: ServiceIdentifier): Option[Peer] = {
    val filteredPeers =
      peerDataMap
        .filter(p => p._2.serviceIdentifier.hasServicesOf(services))
        .keys
        .toVector
    val (good, _) =
      filteredPeers.partition(p => !peerDataMap(p).hasFailedRecently)

    val peerOpt = if (good.nonEmpty) {
      Some(good(Random.nextInt(good.length)))
    } else {
      None
    }
    peerOpt
  }

  private def createInDb(
      peer: Peer,
      serviceIdentifier: ServiceIdentifier): Future[PeerDb] = {
    logger.debug(s"Adding peer to db $peer")
    val addrBytes =
      if (peer.socket.getHostString.contains(".onion"))
        NetworkUtil.torV3AddressToBytes(peer.socket.getHostString)
      else
        InetAddress.getByName(peer.socket.getHostString).getAddress
    val networkByte = addrBytes.length match {
      case AddrV2Message.IPV4_ADDR_LENGTH   => AddrV2Message.IPV4_NETWORK_BYTE
      case AddrV2Message.IPV6_ADDR_LENGTH   => AddrV2Message.IPV6_NETWORK_BYTE
      case AddrV2Message.TOR_V3_ADDR_LENGTH => AddrV2Message.TOR_V3_NETWORK_BYTE
      case unknownSize =>
        throw new IllegalArgumentException(
          s"Unsupported address type of size $unknownSize bytes")
    }
    PeerDAO()
      .upsertPeer(ByteVector(addrBytes),
                  peer.socket.getPort,
                  networkByte,
                  serviceIdentifier)
  }

  private def replacePeer(replacePeer: Peer, withPeer: Peer): Future[Unit] = {
    logger.debug(s"Replacing $replacePeer with $withPeer")
    assert(!peerDataMap(replacePeer).serviceIdentifier.nodeCompactFilters,
           s"$replacePeer has cf")
    for {
      _ <- disconnectPeer(replacePeer)
      _ <- connectPeer(withPeer)
    } yield {
      ()
    }
  }

  def disconnectPeer(peer: Peer): Future[Unit] = {
    logger.debug(s"Disconnecting persistent peer=$peer")
    offer(InitializeDisconnect(peer)).map(_ => ())
  }

  override def start(): Future[PeerManager] = {
    logger.debug(s"Starting PeerManager")
    val (queue, source) = dataMessageStreamSource.preMaterialize()
    val initState =
      DoneSyncing(peers = peers, waitingForDisconnection = Set.empty)
    val graph =
      buildDataMessageStreamGraph(initState = initState, source = source)
    dataMessageQueueOpt = Some(queue)
    val stateF = graph.run()
    streamDoneFOpt = Some(stateF)
    val finder = PeerFinder(
      paramPeers = paramPeers,
      peerManager = this,
      skipPeers = () => peers
    )
    finderOpt = Some(finder)

    val inactivityCancellable = startInactivityChecksJob()
    inactivityCancellableOpt = Some(inactivityCancellable)
    finder.start().map { _ =>
      logger.info("Done starting PeerManager")
      isStarted.set(true)
      this
    }
  }

  private def peerDataMap: Map[Peer, PersistentPeerData] = _peerDataMap.toMap

  def getPeerData(peer: Peer): Option[PersistentPeerData] =
    peerDataMap.get(peer)

  override def stop(): Future[PeerManager] = {
    logger.info(s"Stopping PeerManager")
    isStarted.set(false)
    val beganAt = System.currentTimeMillis()

    syncFilterCancellableOpt.map(_.cancel())

    inactivityCancellableOpt.map(_.cancel())

    val finderStopF = finderOpt match {
      case Some(finder) => finder.stop()
      case None         => Future.unit
    }

    val stopF = for {
      _ <- finderStopF
      _ <- Future.traverse(peers)(disconnectPeer)
      _ <- AsyncUtil.retryUntilSatisfied(
        _peerDataMap.isEmpty,
        interval = 1.seconds,
        maxTries = 30
      )
      _ = dataMessageQueueOpt.map(_.complete())
      _ <- {
        val finishedF = streamDoneFOpt match {
          case Some(f) => f
          case None    => Future.successful(Done)
        }
        finishedF
      }
      _ <- watchCompletion()
      _ = {
        //reset all variables
        syncFilterCancellableOpt = None
        dataMessageQueueOpt = None
        streamDoneFOpt = None
        finderOpt = None
        inactivityCancellableOpt = None
      }
    } yield {
      logger.info(
        s"Stopped PeerManager. Took ${System.currentTimeMillis() - beganAt} ms ")
      this
    }

    stopF
  }

  def isConnected(peer: Peer): Future[Boolean] = {
    peerDataMap.get(peer) match {
      case None    => Future.successful(false)
      case Some(p) => p.peerConnection.isConnected()
    }
  }

  def isDisconnected(peer: Peer): Future[Boolean] = {
    isConnected(peer).map(b => !b)
  }

  def isInitialized(peer: Peer): Future[Boolean] = {
    Future.successful(peerDataMap.exists(_._1 == peer))
  }

  private def onInitializationTimeout(peer: Peer): Future[Unit] = {
    finderOpt match {
      case Some(finder) =>
        require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
                s"$peer cannot be both a test and a persistent peer")

        if (finder.hasPeer(peer)) {
          //one of the peers that we tried, failed to init within time, disconnect
          finder.getData(peer).get.stop()
        } else if (peerDataMap.contains(peer)) {
          //this is one of our persistent peers which must have been initialized earlier, this can happen in case of
          //a reconnection attempt, meaning it got connected but failed to initialize, disconnect
          peerDataMap(peer).stop()
        } else {
          //this should never happen
          logger.warn(s"onInitializationTimeout called for unknown $peer")
          Future.unit
        }
      case None =>
        logger.warn(
          s"Cannot execute onInitializationTimeout, finder not started")
        Future.unit
    }

  }

  /** Helper method to determine what action to take after a peer is initialized, such as beginning sync with that peer */
  private def managePeerAfterInitialization(
      finder: PeerFinder,
      peerData: PeerData,
      hasCf: Boolean): Future[Unit] = {
    val peer = peerData.peer

    peerData match {
      case _: PersistentPeerData =>
        //if we have slots remaining, connect
        if (connectedPeerCount < nodeAppConfig.maxConnectedPeers) {
          connectPeer(peer)
            .flatMap(_ => syncHelper(Some(peer)))
        } else {
          val notCf = peerDataMap
            .filter(p => !p._2.serviceIdentifier.nodeCompactFilters)
            .keys

          //try to drop another non compact filter connection for this
          if (hasCf && notCf.nonEmpty)
            replacePeer(replacePeer = notCf.head, withPeer = peer)
              .flatMap(_ => syncHelper(Some(peer)))
          else {
            peerData.stop()
          }
        }
      case q: AttemptToConnectPeerData =>
        if (finder.hasPeer(q.peer)) {
          //if we still have an active connection with this peer, stop it
          q.stop()
        } else {
          //else it already has been deleted because of connection issues
          Future.unit
        }
    }

  }

  private def onInitialization(
      peer: Peer,
      state: NodeState): Future[NodeState] = {
    val stateF: Future[NodeState] = finderOpt match {
      case Some(finder) =>
        require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
                s"$peer cannot be both a test and a persistent peer")

        //this assumes neutrino and checks for compact filter support so should not be called for anything else
        require(nodeAppConfig.nodeType == NodeType.NeutrinoNode,
                s"Node cannot be ${nodeAppConfig.nodeType.shortName}")

        if (finder.hasPeer(peer)) {
          //one of the peers we tries got initialized successfully
          val peerData = finder.getData(peer).get
          val peerMsgSender = PeerMessageSender(peerData.peerConnection)
          val serviceIdentifer = peerData.serviceIdentifier
          val hasCf = serviceIdentifer.nodeCompactFilters
          logger.debug(s"Initialized peer $peer with $hasCf")

          for {
            _ <- peerMsgSender.sendGetAddrMessage()
            _ <- createInDb(peer, peerData.serviceIdentifier)
            _ <- managePeerAfterInitialization(finder = finder,
                                               peerData = peerData,
                                               hasCf = hasCf)
          } yield state

        } else if (peerDataMap.contains(peer)) {
          //one of the persistent peers initialized again, this can happen in case of a reconnection attempt
          //which succeeded which is all good, do nothing
          syncHelper(Some(peer)).map(_ => state)
        } else {
          logger.warn(s"onInitialization called for unknown $peer")
          Future.successful(state)
        }
      case None =>
        logger.warn(
          s"onInitialization cannot be run, PeerFinder was not started")
        Future.successful(state)
    }

    stateF.map(_.replacePeers(peers))
  }

  /** @param peer the peer we were disconencted from
    * @param reconnect flag indicating if we should attempt to reconnect
    * @return
    */
  private def onDisconnect(
      peer: Peer,
      forceReconnect: Boolean,
      state: NodeState): Future[NodeState] = {
    logger.info(
      s"Disconnected peer=$peer peers=$peers state=$state forceReconnect=$forceReconnect finder.isDefined=${finderOpt.isDefined} peerDataMap=${peerDataMap
        .map(_._1)}")
    val stateF = finderOpt match {
      case Some(finder) =>
        require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
                s"$peer cannot be both a test and a persistent peer")

        if (finder.hasPeer(peer)) {
          //client actor for one of the test peers stopped, can remove it from map now
          finder.removePeer(peer).map(_ => state)
        } else if (peerDataMap.contains(peer)) {
          val peerData = _peerDataMap(peer)
          _peerDataMap.remove(peer)
          peerData.stop().flatMap { _ =>
            //getDataMesageHandler.state is already mutated from another thread
            //this will be set to the new sync peer not the old one.
            val syncPeerOpt = state match {
              case s: SyncNodeState =>
                Some(s.syncPeer)
              case m: MisbehavingPeer => Some(m.badPeer)
              case _: DoneSyncing | _: RemovePeers =>
                None
            }
            val shouldReconnect =
              (forceReconnect || connectedPeerCount == 0) && isStarted.get
            if (peers.exists(_ != peer)) {
              val randomPeerOpt =
                randomPeerWithService(ServiceIdentifier.NODE_COMPACT_FILTERS)
              randomPeerOpt match {
                case Some(peer) =>
                  state match {
                    case syncState: SyncNodeState =>
                      switchSyncToPeer(oldSyncState = syncState, newPeer = peer)
                    case d: DoneSyncing =>
                      //defensively try to sync with the new peer
                      syncHelper(Some(peer)).map(_ => d)
                    case x @ (_: MisbehavingPeer | _: RemovePeers) =>
                      Future.successful(x)
                  }
                case None =>
                  //if we have no new peers should we just switch to DoneSyncing?
                  Future.successful(state)
              }
            } else if (syncPeerOpt.isDefined) {
              if (shouldReconnect) {
                finder.reconnect(peer).map(_ => state)
              } else {
                val exn = new RuntimeException(
                  s"No new peers to sync from, cannot start new sync. Terminated sync with peer=$peer current syncPeer=$syncPeerOpt state=${state} peers=$peers")
                Future.failed(exn)
              }
            } else {
              if (shouldReconnect) {
                finder.reconnect(peer).map(_ => state)
              } else {
                Future.successful(state)
              }
            }
          }
        } else if (state.waitingForDisconnection.contains(peer)) {
          //a peer we wanted to disconnect has remove has stopped the client actor, finally mark this as deleted
          val removed = state.waitingForDisconnection.removedAll(Set(peer))
          Future.successful(state.replaceWaitingForDisconnection(removed))
        } else {
          logger.warn(s"onP2PClientStopped called for unknown $peer")
          Future.successful(state)
        }
      case None =>
        logger.warn(
          s"onP2PClientStopped cannot be run, PeerFinder was not started")
        Future.successful(state)
    }

    stateF.map {
      case s: SyncNodeState =>
        if (s.syncPeer == peer) {
          //the peer being disconnected is our sync peer
          randomPeerWithService(ServiceIdentifier.NODE_COMPACT_FILTERS) match {
            case Some(p) => s.replaceSyncPeer(p)
            case None    =>
              //switch to state DoneSyncing since we have no peers to sync from
              DoneSyncing(peers, state.waitingForDisconnection)
          }
        } else {
          s.replacePeers(peers)
        }
      case s @ (_: RemovePeers | _: MisbehavingPeer | _: DoneSyncing) =>
        s.replacePeers(peers)
    }
  }

  def onVersionMessage(peer: Peer, versionMsg: VersionMessage): Unit = {
    finderOpt match {
      case Some(finder) =>
        require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
                s"$peer cannot be both a test and a persistent peer")

        if (finder.hasPeer(peer)) {
          finder.getData(peer).get.setServiceIdentifier(versionMsg.services)
        } else if (peerDataMap.contains(peer)) {
          require(
            peerDataMap(
              peer).serviceIdentifier.bytes == versionMsg.services.bytes)
        } else {
          logger.warn(s"onVersionMessage called for unknown $peer")
        }
      case None =>
        logger.warn(
          s"onVersionMessage cannot be run, PeerFinder was not started")
        ()
    }

  }

  private def onQueryTimeout(
      payload: ExpectsResponse,
      peer: Peer,
      state: NodeState): Future[Unit] = {
    logger.debug(s"Query timeout out for $peer with payload=${payload}")

    //if we are removing this peer and an existing query timed out because of that
    // peerData will not have this peer
    if (peerDataMap.contains(peer)) {
      peerDataMap(peer).updateLastFailureTime()
    }

    payload match {
      case _: GetHeadersMessage =>
        offer(HeaderTimeoutWrapper(peer)).map(_ => ())
      case _ =>
        val syncPeer = state match {
          case syncState: SyncNodeState =>
            syncState.syncPeer
          case s @ (_: DoneSyncing | _: MisbehavingPeer | _: RemovePeers) =>
            sys.error(s"Cannot have state=$s and have a query timeout")
        }
        if (peer == syncPeer)
          syncFromNewPeer().map(_ => ())
        else Future.unit
    }
  }

  private def onHeaderRequestTimeout(
      peer: Peer,
      state: NodeState): Future[NodeState] = {
    logger.info(s"Header request timed out from $peer in state $state")
    state match {
      case _: HeaderSync | _: MisbehavingPeer | _: DoneSyncing =>
        syncFromNewPeer().map(_ => state)

      case _: FilterHeaderSync | _: FilterSync | _: RemovePeers =>
        Future.successful(state)
    }
  }

  private def sendResponseTimeout(
      peer: Peer,
      payload: NetworkPayload): Future[Unit] = {
    logger.debug(
      s"Sending response timeout for ${payload.commandName} to $peer")
    if (peerDataMap.contains(peer)) {
      payload match {
        case e: ExpectsResponse =>
          offer(QueryTimeout(peer, e))
            .map(_ => ())
        case _: NetworkPayload =>
          val exn = new RuntimeException(
            s"Cannot have sendResponseTimeout for msg=${payload.commandName} for non ExpectsResponse payload")
          Future.failed(exn)
      }
    } else {
      logger.debug(s"Requested to send response timeout for unknown $peer")
      Future.unit
    }
  }

  private val dataMessageStreamSource: Source[
    NodeStreamMessage,
    SourceQueueWithComplete[NodeStreamMessage]] = {
    Source
      .queue[NodeStreamMessage](
        100 * nodeAppConfig.maxConnectedPeers,
        overflowStrategy = OverflowStrategy.backpressure,
        maxConcurrentOffers = Runtime.getRuntime.availableProcessors())
  }

  private def buildP2PMessageHandlerSink(
      initState: NodeState): Sink[NodeStreamMessage, Future[NodeState]] = {
    Sink.foldAsync(initState) {
      case (state, s: StartSync) =>
        syncHelper(s.peerOpt).map(_ => state)
      case (state, i: InitializeDisconnect) =>
        val client: PeerData = peerDataMap(i.peer)
        _peerDataMap.remove(i.peer)
        //so we need to remove if from the map for connected peers so no more request could be sent to it but we before
        //the actor is stopped we don't delete it to ensure that no such case where peers is deleted but actor not stopped
        //leading to a memory leak may happen

        //now send request to stop actor which will be completed some time in future
        client.stop().map { _ =>
          val newWaiting = state.waitingForDisconnection.+(i.peer)
          val newState = state.replaceWaitingForDisconnection(newWaiting)
          newState
        }
      case (state, DataMessageWrapper(payload, peer)) =>
        logger.debug(s"Got ${payload.commandName} from peer=${peer} in stream")
        val peerDataOpt = peerDataMap.get(peer)
        peerDataOpt match {
          case None =>
            logger.warn(
              s"Ignoring received msg=${payload.commandName} from peer=$peer because it was disconnected, peers=$peers state=${state}")
            Future.successful(state)
          case Some(peerData) =>
            val peerMsgSender = PeerMessageSender(peerData.peerConnection)
            val dmh = DataMessageHandler(chainApi = ChainHandler.fromDatabase(),
                                         walletCreationTimeOpt =
                                           walletCreationTimeOpt,
                                         peerMessageSenderApi = peerMsgSender,
                                         peerManager = this,
                                         state = state)
            val resultF = dmh
              .handleDataPayload(payload, peerData)
              .flatMap { newDmh =>
                newDmh.state match {
                  case m: MisbehavingPeer =>
                    //disconnect the misbehaving peer
                    for {
                      _ <- disconnectPeer(m.badPeer)
                      _ <- syncFromNewPeer()
                    } yield newDmh
                  case removePeers: RemovePeers =>
                    for {
                      _ <- Future.traverse(removePeers.peers)(disconnectPeer)
                    } yield newDmh
                  case _: SyncNodeState | _: DoneSyncing =>
                    Future.successful(newDmh)
                }
              }
            resultF.map { r =>
              logger.debug(
                s"Done processing ${payload.commandName} in peer=${peer}")
              r.state
            }
        }
      case (state, ControlMessageWrapper(payload, peer)) =>
        val controlMessageHandlerOpt: Option[ControlMessageHandler] =
          payload match {
            case _: VersionMessage | VerAckMessage =>
              finderOpt.flatMap(_.getData(peer)).map { peerData =>
                peerData.controlMessageHandler
              }
            case _: ControlPayload =>
              val peerMsgSenderOpt = getPeerMsgSender(peer)
              peerMsgSenderOpt.map(p => ControlMessageHandler(this, p))
          }
        controlMessageHandlerOpt match {
          case Some(controlMessageHandler) =>
            controlMessageHandler.handleControlPayload(payload, peer).flatMap {
              case Some(i) =>
                onInitialization(i.peer, state)
              case None =>
                Future.successful(state)
            }
          case None =>
            logger.warn(
              s"Cannot find peer=$peer to handle control payload=${payload.commandName}")
            Future.successful(state)
        }

      case (state, HeaderTimeoutWrapper(peer)) =>
        logger.debug(s"Processing timeout header for $peer")
        for {
          newDmh <- {
            onHeaderRequestTimeout(peer, state).map { s =>
              logger.debug(s"Done processing timeout header for $peer")
              s
            }
          }
        } yield newDmh
      case (state, DisconnectedPeer(peer, forceReconnect)) =>
        onDisconnect(peer, forceReconnect, state)
      case (state, i: InitializationTimeout) =>
        onInitializationTimeout(i.peer).map(_ => state)
      case (state, q: QueryTimeout) =>
        onQueryTimeout(q.payload, q.peer, state).map(_ => state)
      case (dmh, srt: SendResponseTimeout) =>
        sendResponseTimeout(srt.peer, srt.payload).map(_ => dmh)
    }
  }

  private val decider: Supervision.Decider = { case err: Throwable =>
    logger.error(s"Error occurred while processing p2p pipeline stream", err)
    Supervision.Resume
  }

  private def buildDataMessageStreamGraph(
      initState: NodeState,
      source: Source[NodeStreamMessage, NotUsed]): RunnableGraph[
    Future[NodeState]] = {
    val graph = source
      .toMat(buildP2PMessageHandlerSink(initState))(Keep.right)
      .withAttributes(ActorAttributes.supervisionStrategy(decider))
    graph
  }

  private[bitcoins] var dataMessageQueueOpt: Option[
    SourceQueueWithComplete[NodeStreamMessage]] = None

  private var streamDoneFOpt: Option[Future[NodeState]] = None

  override def offer(elem: NodeStreamMessage): Future[QueueOfferResult] = {
    dataMessageQueueOpt match {
      case Some(queue) => queue.offer(elem)
      case None =>
        Future.failed(new RuntimeException(
          s"PeerManager not started, cannot process p2p message until PeerManager.start() is called"))
    }
  }

  override def watchCompletion(): Future[Done] = {
    dataMessageQueueOpt match {
      case Some(queue) => queue.watchCompletion()
      case None        => Future.successful(Done)
    }
  }

  private def switchSyncToPeer(
      oldSyncState: SyncNodeState,
      newPeer: Peer): Future[NodeState] = {
    logger.debug(
      s"switchSyncToPeer() oldSyncState=$oldSyncState newPeer=$newPeer")
    val newState = oldSyncState.replaceSyncPeer(newPeer)
    oldSyncState match {
      case s: HeaderSync =>
        if (s.syncPeer != newPeer) {
          syncHelper(syncPeerOpt = Some(newPeer)).map(_ => newState)
        } else {
          //if its same peer we don't need to switch
          Future.successful(oldSyncState)
        }
      case s @ (_: FilterHeaderSync | _: FilterSync) =>
        if (s.syncPeer != newPeer) {
          filterSyncHelper(chainApi = ChainHandler.fromDatabase(),
                           syncPeer = newPeer).map(_ => newState)
        } else {
          //if its same peer we don't need to switch
          Future.successful(oldSyncState)
        }

    }
  }

  /** If [[syncPeerOpt]] is given, we send getheaders to only that peer, if no sync peer given we gossip getheaders to all our peers */
  private def getHeaderSyncHelper(syncPeerOpt: Option[Peer]): Future[Unit] = {
    val blockchainsF =
      BlockHeaderDAO()(ec, chainAppConfig).getBlockchains()

    for {
      blockchains <- blockchainsF
      // Get all of our cached headers in case of a reorg
      cachedHeaders = blockchains.flatMap(_.headers).map(_.hashBE)
      _ <- {
        syncPeerOpt match {
          case Some(peer) =>
            val peerMsgSender = getPeerMsgSender(peer).get //check this .get
            peerMsgSender.sendGetHeadersMessage(cachedHeaders)
          case None => gossipGetHeadersMessage(cachedHeaders)
        }
      }
    } yield ()
  }

  private def filterSyncHelper(
      chainApi: ChainApi,
      syncPeer: Peer): Future[Unit] = {
    for {
      header <- chainApi.getBestBlockHeader()
      bestFilterHeaderOpt <- chainApi.getBestFilterHeader()
      bestFilterOpt <- chainApi.getBestFilter()

      hasStaleTip <- chainApi.isTipStale()
      _ <- {
        if (hasStaleTip) {
          //if we have a stale tip, we will request to sync filter headers / filters
          //after we are done syncing block headers
          Future.unit
        } else {
          val fhs = FilterHeaderSync(syncPeer = syncPeer,
                                     peers = peers,
                                     waitingForDisconnection = Set.empty)
          syncFilters(
            bestFilterHeaderOpt = bestFilterHeaderOpt,
            bestFilterOpt = bestFilterOpt,
            bestBlockHeader = header,
            chainApi = chainApi,
            nodeState = fhs
          )
        }
      }
    } yield ()
  }

  /** Scheduled job to sync compact filters */
  @volatile private[this] var syncFilterCancellableOpt: Option[Cancellable] =
    None

  def sync(syncPeerOpt: Option[Peer]): Future[Unit] = {
    val s = StartSync(syncPeerOpt)
    offer(s).map(_ => ())
  }

  /** Helper method to sync the blockchain over the network
    *
    * @param syncPeerOpt if syncPeer is given, we send [[org.bitcoins.core.p2p.GetHeadersMessage]] to that peer. If None we gossip GetHeadersMessage to all peers
    */
  private def syncHelper(syncPeerOpt: Option[Peer]): Future[Unit] = {
    logger.debug(
      s"syncHelper() syncPeerOpt=$syncPeerOpt isStarted.get=${isStarted.get} syncFilterCancellableOpt.isDefined=${syncFilterCancellableOpt.isDefined}")
    val chainApi: ChainApi = ChainHandler.fromDatabase()
    val headerF = chainApi.getBestBlockHeader()
    val filterHeaderCountF = chainApi.getFilterHeaderCount()
    val filterCountF = chainApi.getFilterCount()
    for {
      _ <- chainApi.setSyncing(true)
      _ <- getHeaderSyncHelper(syncPeerOpt)
      _ = {
        if (isStarted.get) {
          //in certain cases, we can schedule this job while the peer manager is attempting to shutdown
          //this is because we start syncing _after_ the connection to the peer is established
          //while we are waiting for this connection to be established, we could decide to shutdown the PeerManager
          //if we are unlucky there could be a race condition here between
          //
          // 1. Starting to sync blockchain data from our peer we just established a connection with
          // 2. Shutting down the peer manager.
          //
          // the filter sync job gets scheduled _after_ PeerManager.stop() has been called
          syncFilterCancellableOpt = syncFilterCancellableOpt match {
            case s: Some[Cancellable] =>
              s //do nothing as we already have a job scheduled
            case None =>
              syncPeerOpt match {
                case Some(syncPeer) =>
                  val c = createFilterSyncJob(chainApi, syncPeer)
                  Some(c)
                case None =>
                  //no sync peer to schedule the job with
                  logger.warn(
                    s"Unable to createFilterSyncJob as we have no sync peer!")
                  None
              }
          }
        }
      }
      header <- headerF
      filterHeaderCount <- filterHeaderCountF
      filterCount <- filterCountF
    } yield {
      logger.info(
        s"Starting sync node, height=${header.height} hash=${header.hashBE.hex} filterHeaderCount=$filterHeaderCount filterCount=$filterCount peerOpt=$syncPeerOpt")
    }
  }

  private def createFilterSyncJob(
      chainApi: ChainApi,
      syncPeer: Peer): Cancellable = {
    require(
      syncFilterCancellableOpt.isEmpty,
      s"Cannot schedule a syncFilterCancellable as one is already scheduled")
    //add a delay when syncing filter headers/filters for the case when we restart the node,
    //our block header tip _is not_ synced with the network, but our tip is also _not_ stale
    //this can result in duplicate syncing of filter headers.
    //see: https://github.com/bitcoin-s/bitcoin-s/issues/5125
    val oldFilterHeaderCountF = chainApi.getFilterHeaderCount()
    val oldFilterCountF = chainApi.getFilterCount()
    system.scheduler.scheduleOnce(10.seconds) {
      val filterSyncF = {
        for {
          oldFilterHeaderCount <- oldFilterHeaderCountF
          oldFilterCount <- oldFilterCountF
          blockCount <- chainApi.getBlockCount()
          currentFilterHeaderCount <- chainApi.getFilterHeaderCount()
          currentFilterCount <- chainApi.getFilterCount()
          _ <- {
            //make sure filter sync hasn't started since we schedule the job...
            //see: https://github.com/bitcoin-s/bitcoin-s/issues/5167
            val isOutOfSync = PeerManager.isFiltersOutOfSync(
              blockCount = blockCount,
              oldFilterHeaderCount = oldFilterHeaderCount,
              currentFilterHeaderCount = currentFilterHeaderCount,
              oldFilterCount = oldFilterCount,
              currentFilterCount = currentFilterCount
            )

            if (isOutOfSync) {
              //if it hasn't started it, start it
              filterSyncHelper(chainApi, syncPeer)
            } else {
              Future.unit
            }
          }
        } yield ()
      }
      filterSyncF.onComplete {
        case scala.util.Success(_) =>
          syncFilterCancellableOpt = None
        case scala.util.Failure(err) =>
          logger.error(s"Failed to start syncing filters", err)
          syncFilterCancellableOpt = None
      }
      ()
    }
  }

  /** Returns true if filter are in sync with their old counts, but out of sync with our block count */

  private def syncFilters(
      bestFilterHeaderOpt: Option[CompactFilterHeaderDb],
      bestFilterOpt: Option[CompactFilterDb],
      bestBlockHeader: BlockHeaderDb,
      chainApi: ChainApi,
      nodeState: SyncNodeState): Future[Unit] = {
    val isTipStaleF = chainApi.isTipStale()
    isTipStaleF.flatMap { isTipStale =>
      if (isTipStale) {
        logger.error(
          s"Cannot start syncing filters while blockchain tip is stale")
        Future.unit
      } else {
        logger.debug(
          s"syncFilters() bestBlockHeader=$bestBlockHeader bestFilterHeaderOpt=$bestFilterHeaderOpt bestFilterOpt=$bestFilterOpt state=$nodeState")
        // If we have started syncing filters headers
        (bestFilterHeaderOpt, bestFilterOpt) match {
          case (None, None) | (None, Some(_)) =>
            nodeState match {
              case fhs: FilterHeaderSync =>
                val peerMsgSender =
                  getPeerMsgSender(fhs.syncPeer).get //check this .get
                PeerManager
                  .sendFirstGetCompactFilterHeadersCommand(
                    peerMessageSenderApi = peerMsgSender,
                    chainApi = chainApi,
                    state = fhs)
                  .map(_ => ())
              case x @ (_: FilterSync | _: HeaderSync) =>
                val exn = new RuntimeException(
                  s"Invalid state to start syncing filter headers with, got=$x")
                Future.failed(exn)
            }

          case (Some(bestFilterHeader), Some(bestFilter)) =>
            val isFilterHeaderSynced =
              bestFilterHeader.blockHashBE == bestBlockHeader.hashBE
            val isFiltersSynced = {
              //check if we have started syncing filters,
              //and if so, see if filter headers and filters
              //were in sync
              bestFilter.hashBE == bestFilterHeader.filterHashBE
            }
            if (isFilterHeaderSynced && isFiltersSynced) {
              //means we are in sync, with filter heads & block headers & filters
              //if there _both_ filter headers and block headers are on
              //an old tip, our event driven node will start syncing
              //filters after block headers are in sync
              //do nothing
              Future.unit
            } else {
              syncCompactFilters(bestFilterHeader = bestFilterHeader,
                                 chainApi = chainApi,
                                 compactFilterStartHeight = bestFilter.height,
                                 nodeState = nodeState)
            }
          case (Some(bestFilterHeader), None) =>
            val compactFilterStartHeightF =
              PeerManager.getCompactFilterStartHeight(chainApi,
                                                      walletCreationTimeOpt)
            for {
              compactFilterStartHeight <- compactFilterStartHeightF
              _ <- syncCompactFilters(bestFilterHeader = bestFilterHeader,
                                      chainApi = chainApi,
                                      compactFilterStartHeight =
                                        compactFilterStartHeight,
                                      nodeState = nodeState)
            } yield ()

        }
      }
    }
  }

  private def syncFromNewPeer(): Future[Option[Peer]] = {
    val syncPeerOpt = randomPeerWithService(
      ServiceIdentifier.NODE_COMPACT_FILTERS)
    for {
      _ <- syncHelper(syncPeerOpt)
    } yield syncPeerOpt
  }

  /** Gossips the given message to all peers except the excluded peer. If None given as excluded peer, gossip message to all peers */
  override def gossipMessage(
      msg: NetworkPayload,
      excludedPeerOpt: Option[Peer]): Future[Unit] = {
    val gossipPeers = excludedPeerOpt match {
      case Some(excludedPeer) =>
        peerDataMap
          .filterNot(_._1 == excludedPeer)
          .map(_._2)
      case None => peerDataMap.map(_._2)
    }
    if (gossipPeers.isEmpty) {
      logger.warn(
        s"We have 0 peers to gossip message=${msg.commandName} to peerDataMap=${peerDataMap
          .map(_._1)}.")
      Future.unit
    } else {
      Future
        .traverse(gossipPeers) { p =>
          val sender = PeerMessageSender(p.peerConnection)
          sender.sendMsg(msg)
        }
        .map(_ => ())
    }
  }

  override def gossipGetHeadersMessage(
      hashes: Vector[DoubleSha256DigestBE]): Future[Unit] = {
    val headersMsg = GetHeadersMessage(hashes.distinct.take(101).map(_.flip))
    gossipMessage(msg = headersMsg, excludedPeerOpt = None)
  }

  override def sendToRandomPeer(payload: NetworkPayload): Future[Unit] = {
    val randomPeerOpt = randomPeerConnection(
      ServiceIdentifier.NODE_COMPACT_FILTERS)
    randomPeerOpt match {
      case Some(p) =>
        val peerMsgSender = PeerMessageSender(p)
        peerMsgSender.sendMsg(payload)
      case None =>
        logger.warn(s"Cannot find peer to send message=${payload.commandName}")
        Future.unit
    }
  }

  private[this] val INACTIVITY_CHECK_TIMEOUT = 60.seconds

  @volatile private[this] var inactivityCancellableOpt: Option[Cancellable] =
    None

  private def inactivityChecks(peerData: PersistentPeerData): Future[Unit] = {
    if (peerData.isConnectionTimedOut) {
      val stopF = peerData.stop()
      stopF
    } else {
      Future.unit
    }
  }

  private def inactivityChecksRunnable(): Runnable = { () =>
    logger.info(s"Running inactivity checks for peers=${peerDataMap.map(_._1)}")
    val resultF = if (peerDataMap.nonEmpty) {
      Future
        .traverse(peerDataMap.map(_._2))(inactivityChecks)
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
}

case class ResponseTimeout(payload: NetworkPayload)

object PeerManager extends Logging {

  /** Sends first getcfheader message.
    * Returns None if are our filter headers are in sync with our block headers
    */
  def sendFirstGetCompactFilterHeadersCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      chainApi: ChainApi,
      state: SyncNodeState)(implicit
      ec: ExecutionContext,
      chainConfig: ChainAppConfig): Future[Option[NodeState]] = {
    for {
      bestFilterHeaderOpt <-
        chainApi
          .getBestFilterHeader()
      blockHash = bestFilterHeaderOpt match {
        case Some(filterHeaderDb) =>
          filterHeaderDb.blockHashBE
        case None =>
          DoubleSha256DigestBE.empty
      }
      hashHeightOpt <- chainApi.nextBlockHeaderBatchRange(
        prevStopHash = blockHash,
        batchSize = chainConfig.filterHeaderBatchSize)
      res <- hashHeightOpt match {
        case Some(filterSyncMarker) =>
          peerMessageSenderApi
            .sendGetCompactFilterHeadersMessage(filterSyncMarker)
            .map(_ =>
              Some(
                FilterHeaderSync(state.syncPeer,
                                 state.peers,
                                 state.waitingForDisconnection)))
        case None =>
          logger.info(
            s"Filter headers are synced! filterHeader.blockHashBE=$blockHash")
          Future.successful(None)
      }
    } yield res
  }

  def sendNextGetCompactFilterHeadersCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      chainApi: ChainApi,
      peer: Peer,
      filterHeaderBatchSize: Int,
      prevStopHash: DoubleSha256DigestBE)(implicit
      ec: ExecutionContext): Future[Boolean] = {
    for {
      filterSyncMarkerOpt <- chainApi.nextBlockHeaderBatchRange(
        prevStopHash = prevStopHash,
        batchSize = filterHeaderBatchSize)
      res <- filterSyncMarkerOpt match {
        case Some(filterSyncMarker) =>
          logger.info(
            s"Requesting next compact filter headers from $filterSyncMarker with peer=$peer")
          peerMessageSenderApi
            .sendGetCompactFilterHeadersMessage(filterSyncMarker)
            .map(_ => true)
        case None =>
          Future.successful(false)
      }
    } yield res
  }

  /** @return a flag indicating if we are syncing or not
    */
  def sendNextGetCompactFilterCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      chainApi: ChainApi,
      filterBatchSize: Int,
      startHeight: Int,
      peer: Peer)(implicit ec: ExecutionContext): Future[Boolean] = {
    for {
      filterSyncMarkerOpt <-
        chainApi.nextFilterHeaderBatchRange(startHeight, filterBatchSize)
      res <- filterSyncMarkerOpt match {
        case Some(filterSyncMarker) =>
          logger.info(
            s"Requesting compact filters from $filterSyncMarker with peer=$peer")

          peerMessageSenderApi
            .sendGetCompactFiltersMessage(filterSyncMarker)
            .map(_ => true)
        case None =>
          Future.successful(false)
      }
    } yield res
  }

  def fetchCompactFilterHeaders(
      currentDmh: DataMessageHandler,
      peerMessageSenderApi: PeerMessageSenderApi)(implicit
      ec: ExecutionContext,
      nodeAppConfig: NodeAppConfig,
      chainAppConfig: ChainAppConfig): Future[DataMessageHandler] = {
    val syncNodeState = currentDmh.state match {
      case s: SyncNodeState => s
      case state @ (_: DoneSyncing | _: MisbehavingPeer | _: RemovePeers) =>
        sys.error(
          s"Cannot fetch compact filter headers when we are in state=$state")
    }
    logger.info(
      s"Now syncing filter headers from ${syncNodeState.syncPeer} in state=${currentDmh.state}")
    for {
      newSyncingStateOpt <- PeerManager.sendFirstGetCompactFilterHeadersCommand(
        peerMessageSenderApi = peerMessageSenderApi,
        chainApi = currentDmh.chainApi,
        state = syncNodeState)
    } yield {
      newSyncingStateOpt match {
        case Some(newSyncingState) =>
          currentDmh.copy(state = newSyncingState)
        case None =>
          val state = DoneSyncing(currentDmh.state.peers,
                                  currentDmh.state.waitingForDisconnection)
          currentDmh.copy(state = state)
      }
    }
  }

  def isFiltersOutOfSync(
      blockCount: Int,
      oldFilterHeaderCount: Int,
      currentFilterHeaderCount: Int,
      oldFilterCount: Int,
      currentFilterCount: Int): Boolean = {
    (oldFilterHeaderCount == currentFilterHeaderCount && oldFilterCount == currentFilterCount) &&
    (blockCount != currentFilterHeaderCount || blockCount != currentFilterCount)
  }

  /** Return the starting point for first sync of compact filters from the network
    *
    * @param chainApi
    * @param walletCreationTimeOpt the time the wallet was created, we will start syncing form this point if given
    * @param ec
    * @return the start height for compact filters
    */
  def getCompactFilterStartHeight(
      chainApi: ChainApi,
      walletCreationTimeOpt: Option[Instant])(implicit
      ec: ExecutionContext): Future[Int] = {
    chainApi.getBestFilter().flatMap {
      case Some(f) =>
        //we have already started syncing filters, return the height of the last filter seen
        Future.successful(f.height)
      case None =>
        walletCreationTimeOpt match {
          case Some(instant) =>
            val creationTimeHeightF = chainApi
              .epochSecondToBlockHeight(instant.toEpochMilli / 1000)
            val filterCountF = chainApi.getFilterCount()
            for {
              creationTimeHeight <- creationTimeHeightF
              filterCount <- filterCountF
            } yield {
              //filterHeightOpt contains the height of the last filter of the last batch
              //so if we want to start syncing filters from the correct height we need to
              //decrease the computed height
              val height = Math.max(0, creationTimeHeight - 1)
              //want to choose the maximum out of these too
              //if our internal chainstate filter count is > creationTimeHeight
              //we just want to start syncing from our last seen filter
              Math.max(height, filterCount)
            }
          case None =>
            Future.successful(0)
        }
    }

  }
}
