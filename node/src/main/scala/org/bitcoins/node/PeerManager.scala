package org.bitcoins.node

import akka.actor.{ActorRef, ActorSystem, Cancellable, Props}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Sink, Source, SourceQueueWithComplete}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.blockchain.{ChainHandler}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.p2p._
import org.bitcoins.core.util.{NetworkUtil, StartStopAsync}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{Peer, PeerDAO, PeerDb}
import org.bitcoins.node.networking.peer._
import org.bitcoins.node.networking.P2PClientSupervisor
<<<<<<< HEAD
import org.bitcoins.node.networking.peer.DataMessageHandlerState._
=======
import org.bitcoins.node.networking.peer.DataMessageHandlerState.HeaderSync
>>>>>>> 461fb937056 (WIP: Move DataMessageHandler into PeerManager)
import org.bitcoins.node.util.BitcoinSNodeUtil
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
    with P2PLogger {

  private val _peerDataMap: mutable.Map[Peer, PeerData] = mutable.Map.empty

  /** holds peers removed from peerData whose client actors are not stopped yet. Used for runtime sanity checks. */
  private val _waitingForDeletion: mutable.Set[Peer] = mutable.Set.empty
  def waitingForDeletion: Set[Peer] = _waitingForDeletion.toSet

  private val supervisor: ActorRef =
    system.actorOf(Props[P2PClientSupervisor](),
                   name =
                     BitcoinSNodeUtil.createActorName("P2PClientSupervisor"))

  private val finder: PeerFinder =
    PeerFinder(paramPeers = paramPeers,
               node = node,
               skipPeers = () => peers,
               supervisor = supervisor)

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

  def getPeerMsgSender(peer: Peer): Future[Option[PeerMessageSender]] = {
    _peerDataMap.find(_._1 == peer).map(_._2.peerMessageSender) match {
      case Some(peerMsgSender) => peerMsgSender.map(Some(_))
      case None                => Future.successful(None)
    }
  }

  def getPeerHandler(peer: Peer): Future[Option[PeerHandler]] = {
    for {
      peerMsgSenderOpt <- getPeerMsgSender(peer)
    } yield {
      peerMsgSenderOpt.map(p => PeerHandler(p.client, p))
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

  def createInDb(
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
    finder.start().map { _ =>
      logger.info("Done starting PeerManager")
      this
    }
  }

  def peerDataMap: Map[Peer, PeerData] = _peerDataMap.toMap

  override def stop(): Future[PeerManager] = {
    logger.info(s"Stopping PeerManager")
    val beganAt = System.currentTimeMillis()

    val finderStopF = finder.stop()

    peerServicesQueries.foreach(_.cancel()) //reset the peerServicesQueries var?

    val removeF = Future.sequence(peers.map(removePeer))

    val managerStopF = AsyncUtil.retryUntilSatisfied(
      _peerDataMap.isEmpty && waitingForDeletion.isEmpty,
      interval = 1.seconds,
      maxTries = 30)

    val stopF = for {
      _ <- removeF
      _ <- finderStopF
      _ <- managerStopF
    } yield {
      logger.info(
        s"Stopped PeerManager. Took ${System.currentTimeMillis() - beganAt} ms ")
      this
    }

    stopF.failed.foreach { e =>
      logger.error(
        s"Failed to stop peer manager. Peers: $peers, waiting for deletion: $waitingForDeletion",
        e)
    }

    stopF
  }

  def isConnected(peer: Peer): Future[Boolean] = {
    if (peerDataMap.contains(peer))
      peerDataMap(peer).peerMessageSender.flatMap(_.isConnected())
    else Future.successful(false)
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
      finder.getData(peer).stop()
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
      val peerData = finder.getData(peer)
      val serviceIdentifer = peerData.serviceIdentifier
      val hasCf = serviceIdentifer.nodeCompactFilters
      logger.debug(s"Initialized peer $peer with $hasCf")

      def sendAddrReq: Future[Unit] =
        finder.getData(peer).peerMessageSender.flatMap(_.sendGetAddrMessage())

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
                if (finder.hasPeer(peer))
                  finder.getData(peer).stop()
                else Future.unit
              }
          }
        }
      }

      for {
        _ <- sendAddrReq
        peerData = finder.getData(peer)
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

    logger.info(s"Client stopped for $peer")

    if (finder.hasPeer(peer)) {
      //client actor for one of the test peers stopped, can remove it from map now
      finder.removePeer(peer)
      Future.unit
    } else if (peerDataMap.contains(peer)) {
      //actor stopped for one of the persistent peers, can happen in case a reconnection attempt failed due to
      //reconnection tries exceeding the max limit in which the client was stopped to disconnect from it, remove it
      _peerDataMap.remove(peer)
      val syncPeer = getDataMessageHandler.syncPeer
      if (peers.length > 1 && syncPeer.isDefined && syncPeer.get == peer) {
        node.syncFromNewPeer().map(_ => ())
      } else if (syncPeer.isEmpty) {
        Future.unit
      } else {
        val exn = new RuntimeException(
          s"No new peers to sync from, cannot start new sync. Terminated sync with syncPeer=$syncPeer")
        Future.failed(exn)
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
      finder.getData(peer).setServiceIdentifier(versionMsg.services)
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
        dataMessageStream.offer(HeaderTimeoutWrapper(peer)).map(_ => ())
      case _ =>
        if (peer == getDataMessageHandler.syncPeer.get)
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
      case HeaderSync =>
        syncFromNewPeer()

      case headerState @ ValidatingHeaders(_, failedCheck, _) =>
        val newHeaderState = headerState.copy(failedCheck = failedCheck + peer)
        val newDmh = node.getDataMessageHandler.copy(state = newHeaderState)

        if (newHeaderState.validated) {
          fetchCompactFilterHeaders(newDmh)
            .map(_.copy(state = PostHeaderSync))
        } else Future.successful(newDmh)

      case PostHeaderSync => Future.successful(node.getDataMessageHandler)
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
    .queue[StreamDataMessageWrapper](1500,
                                     overflowStrategy =
                                       OverflowStrategy.backpressure)
    .mapAsync(1) {
      case msg @ DataMessageWrapper(payload, peerMsgSender, peer) =>
        logger.debug(s"Got ${payload.commandName} from peer=${peer} in stream")
        getDataMessageHandler
          .handleDataPayload(payload, peerMsgSender, peer)
          .map { newDmh =>
            updateDataMessageHandler(newDmh)
            msg
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
      case DataMessageWrapper(payload, _, peer) =>
        logger.debug(s"Done processing ${payload.commandName} in peer=${peer}")
      case HeaderTimeoutWrapper(_) =>
    }

  val dataMessageStream: SourceQueueWithComplete[StreamDataMessageWrapper] =
    dataMessageStreamSource.to(dataMessageStreamSink).run()

  def fetchCompactFilterHeaders(
      currentDmh: DataMessageHandler): Future[DataMessageHandler] = {
    for {
      peer <- randomPeerWithService(ServiceIdentifier.NODE_COMPACT_FILTERS)
      newDmh = currentDmh.copy(syncPeer = Some(peer))
      _ = logger.info(s"Now syncing filter headers from $peer")
      sender <- peerDataMap(peer).peerMessageSender
      newSyncing <- PeerManager.sendFirstGetCompactFilterHeadersCommand(
        sender,
        currentDmh.chainApi)
    } yield {
      val syncPeerOpt = if (newSyncing) {
        Some(peer)
      } else {
        None
      }
      newDmh.copy(syncPeer = syncPeerOpt)
    }
  }

  private var dataMessageHandler: DataMessageHandler = {
    DataMessageHandler(
      chainApi = ChainHandler.fromDatabase(),
      walletCreationTimeOpt = walletCreationTimeOpt,
      peerManager = this,
      state = HeaderSync,
      initialSyncDone = None,
      filterBatchCache = Set.empty,
      syncPeer = None
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
      chainConfig: ChainAppConfig): Future[Boolean] = {

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
            .map(_ => true)
        case None =>
          sys.error(
            s"Could not find block header in database to sync filter headers from! It's likely your database is corrupted blockHash=$blockHash bestFilterHeaderOpt=$bestFilterHeaderOpt filterCount=$filterCount")
      }
    } yield res
  }
}
