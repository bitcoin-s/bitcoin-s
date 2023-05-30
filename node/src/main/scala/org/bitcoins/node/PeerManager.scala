package org.bitcoins.node

import akka.Done
import akka.actor.{ActorRef, ActorSystem, Cancellable, Props}
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
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db.{CompactFilterDb, CompactFilterHeaderDb}
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.p2p._
import org.bitcoins.core.util.{NetworkUtil, StartStopAsync}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{Peer, PeerDAO, PeerDb}
import org.bitcoins.node.networking.peer._
import org.bitcoins.node.networking.{P2PClientCallbacks, P2PClientSupervisor}
import org.bitcoins.node.networking.peer.DataMessageHandlerState._
import org.bitcoins.node.util.{BitcoinSNodeUtil, PeerMessageSenderApi}
import scodec.bits.ByteVector

import java.net.InetAddress
import java.time.{Duration, Instant}
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Random

case class PeerManager(
    paramPeers: Vector[Peer] = Vector.empty,
    node: NeutrinoNode,
    walletCreationTimeOpt: Option[Instant])(implicit
    ec: ExecutionContext,
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig)
    extends StartStopAsync[PeerManager]
    with PeerMessageSenderApi
    with SourceQueue[StreamDataMessageWrapper]
    with P2PLogger {

  private val _peerDataMap: mutable.Map[Peer, PeerData] = mutable.Map.empty

  /** holds peers removed from peerData whose client actors are not stopped yet. Used for runtime sanity checks. */
  private val _waitingForDeletion: mutable.Set[Peer] = mutable.Set.empty
  def waitingForDeletion: Set[Peer] = _waitingForDeletion.toSet

  private val supervisor: ActorRef =
    system.actorOf(Props[P2PClientSupervisor](),
                   name =
                     BitcoinSNodeUtil.createActorName("P2PClientSupervisor"))

  private lazy val p2pClientCallbacks = P2PClientCallbacks(
    onReconnect,
    onStop = onP2PClientStopped,
    onInitializationTimeout = onInitializationTimeout,
    onQueryTimeout = onQueryTimeout,
    sendResponseTimeout = sendResponseTimeout
  )

  private val finder: PeerFinder =
    PeerFinder(
      paramPeers = paramPeers,
      controlMessageHandler = ControlMessageHandler(this),
      peerManager = this,
      p2pClientCallbacks = p2pClientCallbacks,
      skipPeers = () => peers,
      supervisor = supervisor
    )

  def addPeerToTry(peers: Vector[Peer], priority: Int = 0): Unit = {
    finder.addToTry(peers, priority)
  }

  def connectedPeerCount: Int = _peerDataMap.size

  def addPeer(peer: Peer): Future[Unit] = {
    require(finder.hasPeer(peer), s"Unknown $peer marked as usable")
    val curPeerData = finder.popFromCache(peer).get
    _peerDataMap.put(peer, curPeerData)
    val hasCf =
      if (curPeerData.serviceIdentifier.nodeCompactFilters) "with filters"
      else ""
    logger.info(
      s"Connected to peer $peer $hasCf. Connected peer count $connectedPeerCount")
    Future.unit
  }

  def peers: Vector[Peer] = _peerDataMap.keys.toVector

  def peerMsgSendersF: Future[Vector[PeerMessageSender]] = {
    Future
      .traverse(_peerDataMap.values)(_.peerMessageSender)
      .map(_.toVector)
  }

  override def sendMsg(
      msg: NetworkPayload,
      peerOpt: Option[Peer]): Future[Unit] = {
    val peerMsgSenderF = peerOpt match {
      case Some(peer) =>
        val peerMsgSenderF = peerDataMap(peer).peerMessageSender
        peerMsgSenderF
      case None =>
        val peerMsgSenderF = randomPeerMsgSenderWithService(
          ServiceIdentifier.NODE_NETWORK)
        peerMsgSenderF
    }
    peerMsgSenderF.flatMap(_.sendMsg(msg))
  }

  /** Gossips the given message to all peers except the excluded peer. If None given as excluded peer, gossip message to all peers */
  override def gossipMessage(
      msg: NetworkPayload,
      excludedPeerOpt: Option[Peer]): Future[Unit] = {
    val gossipPeers = excludedPeerOpt match {
      case Some(excludedPeer) =>
        peerDataMap
          .filterNot(_._1 == excludedPeer)
          .map(_._1)
      case None => peerDataMap.map(_._1)
    }

    Future
      .traverse(gossipPeers)(p => sendMsg(msg, Some(p)))
      .map(_ => ())
  }

  override def sendGetHeadersMessage(
      hashes: Vector[DoubleSha256DigestBE],
      peerOpt: Option[Peer]): Future[Unit] = {
    val peerMsgSenderF = peerOpt match {
      case Some(peer) =>
        val peerMsgSenderF = peerDataMap(peer).peerMessageSender
        peerMsgSenderF
      case None =>
        val peerMsgSenderF = randomPeerMsgSenderWithService(
          ServiceIdentifier.NODE_NETWORK)
        peerMsgSenderF
    }
    peerMsgSenderF.flatMap(_.sendGetHeadersMessage(hashes.map(_.flip)))
  }

  override def sendGetDataMessages(
      typeIdentifier: TypeIdentifier,
      hashes: Vector[DoubleSha256DigestBE],
      peerOpt: Option[Peer]): Future[Unit] = {
    peerOpt match {
      case Some(peer) =>
        val peerMsgSenderF = peerDataMap(peer).peerMessageSender
        val flip = hashes.map(_.flip)
        peerMsgSenderF
          .flatMap(_.sendGetDataMessage(typeIdentifier, flip: _*))
      case None =>
        val peerMsgSenderF = randomPeerMsgSenderWithService(
          ServiceIdentifier.NODE_NETWORK)
        peerMsgSenderF.flatMap(
          _.sendGetDataMessage(TypeIdentifier.MsgWitnessBlock,
                               hashes.map(_.flip): _*))

    }
  }

  /** Starts sync compact filer headers.
    * Only starts syncing compact filters if our compact filter headers are in sync with block headers
    */
  def syncCompactFilters(
      bestFilterHeader: CompactFilterHeaderDb,
      chainApi: ChainApi,
      bestFilterOpt: Option[CompactFilterDb])(implicit
      chainAppConfig: ChainAppConfig): Future[Unit] = {
    val syncPeerMsgSenderOptF = {
      getDataMessageHandler.state match {
        case syncState: SyncDataMessageHandlerState =>
          val peerMsgSender =
            peerDataMap(syncState.syncPeer).peerMessageSender
          Some(peerMsgSender)
        case DoneSyncing | _: MisbehavingPeer => None
      }
    }
    val sendCompactFilterHeaderMsgF = syncPeerMsgSenderOptF match {
      case Some(syncPeerMsgSenderF) =>
        syncPeerMsgSenderF.flatMap(
          _.sendNextGetCompactFilterHeadersCommand(
            chainApi = chainApi,
            filterHeaderBatchSize = chainAppConfig.filterHeaderBatchSize,
            prevStopHash = bestFilterHeader.blockHashBE)
        )
      case None => Future.successful(false)
    }
    sendCompactFilterHeaderMsgF.flatMap { isSyncFilterHeaders =>
      // If we have started syncing filters
      if (
        !isSyncFilterHeaders &&
        bestFilterOpt.isDefined &&
        bestFilterOpt.get.hashBE != bestFilterHeader.filterHashBE
      ) {
        syncPeerMsgSenderOptF match {
          case Some(syncPeerMsgSenderF) =>
            //means we are not syncing filter headers, and our filters are NOT
            //in sync with our compact filter headers
            syncPeerMsgSenderF.flatMap { sender =>
              sender
                .sendNextGetCompactFilterCommand(
                  chainApi = chainApi,
                  filterBatchSize = chainAppConfig.filterBatchSize,
                  startHeight = bestFilterOpt.get.height)
                .map(_ => ())
            }
          case None =>
            logger.warn(
              s"Not syncing compact filters since we do not have a syncPeer set, bestFilterOpt=$bestFilterOpt")
            Future.unit
        }
      } else {
        Future.unit
      }
    }
  }

  def getPeerMsgSender(peer: Peer): Future[Option[PeerMessageSender]] = {
    _peerDataMap.find(_._1 == peer).map(_._2.peerMessageSender) match {
      case Some(peerMsgSender) => peerMsgSender.map(Some(_))
      case None                => Future.successful(None)
    }
  }

  def randomPeerWithService(services: ServiceIdentifier): Future[Peer] = {
    //wait when requested
    val waitF =
      awaitPeerWithService(services,
                           timeout = nodeAppConfig.peerDiscoveryTimeout)

    waitF.map { _ =>
      val filteredPeers =
        peerDataMap
          .filter(p => p._2.serviceIdentifier.hasServicesOf(services))
          .keys
          .toVector
      require(filteredPeers.nonEmpty)
      val (good, failedRecently) =
        filteredPeers.partition(p => !peerDataMap(p).hasFailedRecently)

      if (good.nonEmpty) good(Random.nextInt(good.length))
      else
        failedRecently(Random.nextInt(failedRecently.length))
    }
  }

  def randomPeerMsgSenderWithService(
      services: ServiceIdentifier): Future[PeerMessageSender] = {
    val randomPeerF = randomPeerWithService(services)
    randomPeerF.flatMap(peer => peerDataMap(peer).peerMessageSender)
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

  private var peerServicesQueries: Vector[Cancellable] = Vector.empty

  private def awaitPeerWithService(
      services: ServiceIdentifier,
      timeout: Duration): Future[Unit] = {
    logger.debug(s"Waiting for peer connection. ${_peerDataMap.keys}")
    val promise = Promise[Unit]()
    var counter = 0
    val cancellable =
      system.scheduler.scheduleAtFixedRate(0.seconds, 1.second) { () =>
        if (
          _peerDataMap.exists(x =>
            x._2.serviceIdentifier.hasServicesOf(services))
        ) {
          promise.success(())
        } else if (counter == timeout.getSeconds.toInt) {
          promise.failure(
            new RuntimeException(
              s"No supported peers found! Requested: ${services}"))
        } else {
          counter += 1
        }
      }

    peerServicesQueries = peerServicesQueries.appended(cancellable)

    //remove the cancellable from the peerServicesQueries
    //when our promise is completed from the scheduled job
    promise.future.onComplete { _ =>
      val _: Boolean = cancellable.cancel()
      val idx = peerServicesQueries.indexOf(cancellable)
      if (idx >= 0) {
        peerServicesQueries = peerServicesQueries.zipWithIndex
          .filter(_._2 != idx)
          .map(_._1)
      }
    }
    promise.future
  }

  def replacePeer(replacePeer: Peer, withPeer: Peer): Future[Unit] = {
    logger.debug(s"Replacing $replacePeer with $withPeer")
    assert(!peerDataMap(replacePeer).serviceIdentifier.nodeCompactFilters,
           s"$replacePeer has cf")
    for {
      _ <- removePeer(replacePeer)
      _ <- addPeer(withPeer)
    } yield {
      ()
    }
  }

  def removePeer(peer: Peer): Future[Unit] = {
    logger.debug(s"Removing persistent peer $peer")
    val client: PeerData = peerDataMap(peer)
    _peerDataMap.remove(peer)
    //so we need to remove if from the map for connected peers so no more request could be sent to it but we before
    //the actor is stopped we don't delete it to ensure that no such case where peers is deleted but actor not stopped
    //leading to a memory leak may happen
    _waitingForDeletion.add(peer)
    //now send request to stop actor which will be completed some time in future
    client.stop()
  }

  def isReconnection(peer: Peer): Boolean = {
    peerDataMap.contains(peer)
  }

  override def start(): Future[PeerManager] = {
    logger.debug(s"Starting PeerManager")
    val (queue, doneF) = dataMessageStreamGraph.run()
    dataMessageQueueOpt = Some(queue)
    streamDoneFOpt = Some(doneF)
    finder.start().map { _ =>
      logger.info("Done starting PeerManager")
      this
    }
  }

  private def peerDataMap: Map[Peer, PeerData] = _peerDataMap.toMap

  def getPeerData(peer: Peer): Option[PeerData] = peerDataMap.get(peer)

  override def stop(): Future[PeerManager] = {
    logger.info(s"Stopping PeerManager")
    val beganAt = System.currentTimeMillis()

    val _ = dataMessageQueueOpt.map(_.complete())
    val watchQueueCompleteF = watchCompletion()
    val finderStopF = finder.stop()

    peerServicesQueries.foreach(_.cancel()) //reset the peerServicesQueries var?

    val stopF = for {
      _ <- watchQueueCompleteF
      _ = {
        dataMessageQueueOpt = None //reset dataMessageQueue var
      }
      _ <- Future.traverse(peers)(removePeer)
      _ <- finderStopF
      _ <- AsyncUtil.retryUntilSatisfied(
        _peerDataMap.isEmpty && waitingForDeletion.isEmpty,
        interval = 1.seconds,
        maxTries = 30)
      _ <- {
        val finishedF = streamDoneFOpt match {
          case Some(f) => f
          case None    => Future.successful(Done)
        }
        streamDoneFOpt = None
        finishedF
      }
    } yield {
      logger.info(
        s"Stopped PeerManager. Took ${System.currentTimeMillis() - beganAt} ms ")
      this
    }

    stopF.failed.foreach { e =>
      logger.error(
        s"Failed to stop peer manager. Peers: ${_peerDataMap.map(_._1)}, waiting for deletion: $waitingForDeletion",
        e)
    }

    stopF
  }

  def isConnected(peer: Peer): Future[Boolean] = {
    if (peerDataMap.contains(peer))
      peerDataMap(peer).peerMessageSender.flatMap(_.isConnected())
    else Future.successful(false)
  }

  def isDisconnected(peer: Peer): Future[Boolean] = {
    isConnected(peer).map(b => !b)
  }

  def isInitialized(peer: Peer): Future[Boolean] = {
    if (peerDataMap.contains(peer))
      peerDataMap(peer).peerMessageSender.flatMap(_.isInitialized())
    else Future.successful(false)
  }

  def onInitializationTimeout(peer: Peer): Future[Unit] = {
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
  }

  def onInitialization(peer: Peer): Future[Unit] = {
    require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
            s"$peer cannot be both a test and a persistent peer")

    //this assumes neutrino and checks for compact filter support so should not be called for anything else
    require(nodeAppConfig.nodeType == NodeType.NeutrinoNode,
            s"Node cannot be ${nodeAppConfig.nodeType.shortName}")

    if (finder.hasPeer(peer)) {
      //one of the peers we tries got initialized successfully
      val peerData = finder.getData(peer).get
      val serviceIdentifer = peerData.serviceIdentifier
      val hasCf = serviceIdentifer.nodeCompactFilters
      logger.debug(s"Initialized peer $peer with $hasCf")

      def sendAddrReq: Future[Unit] =
        finder
          .getData(peer)
          .get
          .peerMessageSender
          .flatMap(_.sendGetAddrMessage())

      def managePeerF(): Future[Unit] = {
        //if we have slots remaining, connect
        if (connectedPeerCount < nodeAppConfig.maxConnectedPeers) {
          addPeer(peer)
        } else {
          lazy val notCf = peerDataMap
            .filter(p => !p._2.serviceIdentifier.nodeCompactFilters)
            .keys

          //try to drop another non compact filter connection for this
          if (hasCf && notCf.nonEmpty)
            replacePeer(replacePeer = notCf.head, withPeer = peer)
          else {
            //no use for this apart from writing in db
            //we do want to give it enough time to send addr messages
            AsyncUtil
              .nonBlockingSleep(duration = 10.seconds)
              .flatMap { _ =>
                //could have already been deleted in case of connection issues
                finder.getData(peer) match {
                  case Some(p) => p.stop()
                  case None    => Future.unit
                }
              }
          }
        }
      }

      for {
        _ <- sendAddrReq
        _ <- createInDb(peer, peerData.serviceIdentifier)
        _ <- managePeerF()
      } yield ()

    } else if (peerDataMap.contains(peer)) {
      //one of the persistent peers initialized again, this can happen in case of a reconnection attempt
      //which succeeded which is all good, do nothing
      Future.unit
    } else {
      logger.warn(s"onInitialization called for unknown $peer")
      Future.unit
    }
  }

  def onP2PClientStopped(peer: Peer): Future[Unit] = {
    require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
            s"$peer cannot be both a test and a persistent peer")

    logger.info(s"Client stopped for $peer peers=$peers")

    if (finder.hasPeer(peer)) {
      //client actor for one of the test peers stopped, can remove it from map now
      finder.removePeer(peer)
      Future.unit
    } else if (peerDataMap.contains(peer)) {
      //actor stopped for one of the persistent peers, can happen in case a reconnection attempt failed due to
      //reconnection tries exceeding the max limit in which the client was stopped to disconnect from it, remove it
      _peerDataMap.remove(peer)
      //getDataMesageHandler.state is already mutated from another thread
      //this will be set to the new sync peer not the old one.
      val state = getDataMessageHandler.state
      val syncPeerOpt = state match {
        case s: SyncDataMessageHandlerState =>
          Some(s.syncPeer)
        case m: MisbehavingPeer => Some(m.badPeer)
        case DoneSyncing =>
          None
      }
      if (peers.exists(_ != peer) && syncPeerOpt.isDefined) {
        node.syncFromNewPeer().map(_ => ())
      } else if (syncPeerOpt.isDefined) {
        //means we aren't syncing with anyone, so do nothing?
        val exn = new RuntimeException(
          s"No new peers to sync from, cannot start new sync. Terminated sync with peer=$peer current syncPeer=$syncPeerOpt state=${state}")
        Future.failed(exn)
      } else {
        finder.reconnect(peer)
      }
    } else if (waitingForDeletion.contains(peer)) {
      //a peer we wanted to disconnect has remove has stopped the client actor, finally mark this as deleted
      _waitingForDeletion.remove(peer)
      Future.unit
    } else {
      logger.warn(s"onP2PClientStopped called for unknown $peer")
      Future.unit
    }
  }

  def onVersionMessage(peer: Peer, versionMsg: VersionMessage): Unit = {
    require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
            s"$peer cannot be both a test and a persistent peer")

    if (finder.hasPeer(peer)) {
      finder.getData(peer).get.setServiceIdentifier(versionMsg.services)
    } else if (peerDataMap.contains(peer)) {
      require(
        peerDataMap(peer).serviceIdentifier.bytes == versionMsg.services.bytes)
    } else {
      logger.warn(s"onVersionMessage called for unknown $peer")
    }
  }

  def onQueryTimeout(payload: ExpectsResponse, peer: Peer): Future[Unit] = {
    logger.debug(s"Query timeout out for $peer")

    //if we are removing this peer and an existing query timed out because of that
    // peerData will not have this peer
    if (peerDataMap.contains(peer)) {
      peerDataMap(peer).updateLastFailureTime()
    }

    payload match {
      case _: GetHeadersMessage =>
        offer(HeaderTimeoutWrapper(peer)).map(_ => ())
      case _ =>
        val syncPeer = getDataMessageHandler.state match {
          case syncState: SyncDataMessageHandlerState =>
            syncState.syncPeer
          case s @ (DoneSyncing | _: MisbehavingPeer) =>
            sys.error(s"Cannot have state=$s and have a query timeout")
        }
        if (peer == syncPeer)
          node.syncFromNewPeer().map(_ => ())
        else Future.unit
    }
  }

  def onReconnect(peer: Peer): Future[Unit] = {
    logger.debug(s"Reconnected with $peer")
    Future.unit
  }

  private def onHeaderRequestTimeout(
      peer: Peer,
      state: DataMessageHandlerState): Future[DataMessageHandler] = {
    logger.info(s"Header request timed out from $peer in state $state")
    state match {
      case HeaderSync(_) | MisbehavingPeer(_) =>
        node.syncFromNewPeer().map(_ => getDataMessageHandler)

      case headerState @ ValidatingHeaders(_, _, failedCheck, _) =>
        val newHeaderState = headerState.copy(failedCheck = failedCheck + peer)
        val newDmh = getDataMessageHandler.copy(state = newHeaderState)

        if (newHeaderState.validated) {
          //re-review this
          fetchCompactFilterHeaders(newDmh)
        } else Future.successful(newDmh)

      case DoneSyncing | _: FilterHeaderSync | _: FilterSync =>
        Future.successful(getDataMessageHandler)
    }
  }

  def sendResponseTimeout(peer: Peer, payload: NetworkPayload): Future[Unit] = {
    logger.debug(
      s"Sending response timeout for ${payload.commandName} to $peer")
    if (peerDataMap.contains(peer)) {
      peerDataMap(peer).peerMessageSender.map(
        _.client.actor ! ResponseTimeout(payload))
    } else {
      logger.debug(s"Requested to send response timeout for unknown $peer")
      Future.unit
    }
  }

  private val dataMessageStreamSource = Source
    .queue[StreamDataMessageWrapper](
      8,
      overflowStrategy = OverflowStrategy.backpressure,
      maxConcurrentOffers = nodeAppConfig.maxConnectedPeers)
    .mapAsync(1) {
      case msg @ DataMessageWrapper(payload, peer) =>
        logger.debug(s"Got ${payload.commandName} from peer=${peer} in stream")
        val peerMsgSenderOptF = getPeerMsgSender(peer)
        peerMsgSenderOptF.flatMap {
          case None =>
            Future.failed(new RuntimeException(
              s"Couldn't find PeerMessageSender that corresponds with peer=$peer msg=${payload.commandName}. Was it disconnected?"))
          case Some(peerMsgSender) =>
            getDataMessageHandler
              .handleDataPayload(payload, peerMsgSender, peer)
              .flatMap { newDmh =>
                newDmh.state match {
                  case m: MisbehavingPeer =>
                    updateDataMessageHandler(newDmh)
                    //disconnect the misbehaving peer
                    for {
                      _ <- removePeer(m.badPeer)
                      _ <- node.syncFromNewPeer()
                    } yield msg
                  case _: SyncDataMessageHandlerState | DoneSyncing =>
                    updateDataMessageHandler(newDmh)
                    Future.successful(msg)
                }

              }
        }

      case msg @ HeaderTimeoutWrapper(peer) =>
        logger.debug(s"Processing timeout header for $peer")
        onHeaderRequestTimeout(peer, getDataMessageHandler.state).map {
          newDmh =>
            updateDataMessageHandler(newDmh)
            logger.debug(s"Done processing timeout header for $peer")
            msg
        }
    }

  private val dataMessageStreamSink =
    Sink.foreach[StreamDataMessageWrapper] {
      case DataMessageWrapper(payload, peer) =>
        logger.debug(s"Done processing ${payload.commandName} in peer=${peer}")
      case HeaderTimeoutWrapper(_) =>
    }

  private val decider: Supervision.Decider = { case err: Throwable =>
    logger.error(s"Error occurred while processing p2p pipeline stream", err)
    Supervision.Resume
  }

  private val dataMessageStreamGraph: RunnableGraph[
    (SourceQueueWithComplete[StreamDataMessageWrapper], Future[Done])] = {
    dataMessageStreamSource
      .toMat(dataMessageStreamSink)(Keep.both)
      .withAttributes(ActorAttributes.supervisionStrategy(decider))
  }

  private var dataMessageQueueOpt: Option[
    SourceQueueWithComplete[StreamDataMessageWrapper]] = None

  private var streamDoneFOpt: Option[Future[Done]] = None

  override def offer(
      elem: StreamDataMessageWrapper): Future[QueueOfferResult] = {
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

  def fetchCompactFilterHeaders(
      currentDmh: DataMessageHandler): Future[DataMessageHandler] = {
    val syncPeer = currentDmh.state match {
      case s: SyncDataMessageHandlerState => s.syncPeer
      case state @ (DoneSyncing | _: MisbehavingPeer) =>
        sys.error(
          s"Cannot fetch compact filter headers when we are in state=$state")
    }
    logger.info(
      s"Now syncing filter headers from $syncPeer in state=${currentDmh.state}")
    for {
      sender <- peerDataMap(syncPeer).peerMessageSender
      newSyncingState <- PeerManager.sendFirstGetCompactFilterHeadersCommand(
        sender,
        currentDmh.chainApi)
    } yield {
      currentDmh.copy(state = newSyncingState)
    }
  }

  private var dataMessageHandler: DataMessageHandler = {
    DataMessageHandler(
      chainApi = ChainHandler.fromDatabase(),
      walletCreationTimeOpt = walletCreationTimeOpt,
      peerManager = this,
      state = DoneSyncing,
      filterBatchCache = Set.empty
    )
  }

  def getDataMessageHandler: DataMessageHandler = dataMessageHandler

  def updateDataMessageHandler(
      dataMessageHandler: DataMessageHandler): PeerManager = {
    this.dataMessageHandler = dataMessageHandler
    this
  }

}

case class ResponseTimeout(payload: NetworkPayload)

object PeerManager {

  def sendFirstGetCompactFilterHeadersCommand(
      peerMsgSender: PeerMessageSender,
      chainApi: ChainApi)(implicit
      ec: ExecutionContext,
      chainConfig: ChainAppConfig): Future[DataMessageHandlerState] = {
    val syncPeer = peerMsgSender.client.peer
    for {
      bestFilterHeaderOpt <-
        chainApi
          .getBestFilterHeader()
      filterCount <- chainApi.getFilterCount()
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
          peerMsgSender
            .sendGetCompactFilterHeadersMessage(filterSyncMarker)
            .map(_ => FilterHeaderSync(syncPeer))
        case None =>
          sys.error(
            s"Could not find block header in database to sync filter headers from! It's likely your database is corrupted blockHash=$blockHash bestFilterHeaderOpt=$bestFilterHeaderOpt filterCount=$filterCount")
      }
    } yield res
  }
}
