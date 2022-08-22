package org.bitcoins.node.networking.peer

import akka.Done
import org.bitcoins.chain.blockchain.InvalidBlockHeader
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.gcs.BlockFilter
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models._
import org.bitcoins.node.networking.peer.DataMessageHandlerState._
import org.bitcoins.node.{Node, P2PLogger, PeerManager}

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try
import scala.util.control.NonFatal

/** This actor is meant to handle a [[org.bitcoins.core.p2p.DataPayload DataPayload]]
  * that a peer to sent to us on the p2p network, for instance, if we a receive a
  * [[org.bitcoins.core.p2p.HeadersMessage HeadersMessage]] we should store those headers in our database
  *
  * @param currentFilterBatch holds the current batch of filters to be processed, after its size reaches
  *                           chainConfig.filterBatchSize they will be processed and then emptied
  */
case class DataMessageHandler(
    chainApi: ChainApi,
    walletCreationTimeOpt: Option[Instant],
    node: Node,
    state: DataMessageHandlerState,
    initialSyncDone: Option[Promise[Done]] = None,
    currentFilterBatch: Vector[CompactFilterMessage] = Vector.empty,
    filterHeaderHeightOpt: Option[Int] = None,
    filterHeightOpt: Option[Int] = None,
    syncing: Boolean = false,
    syncPeer: Option[Peer] = None)(implicit
    ec: ExecutionContext,
    appConfig: NodeAppConfig,
    chainConfig: ChainAppConfig)
    extends P2PLogger {

  require(appConfig.nodeType == NodeType.NeutrinoNode,
          "DataMessageHandler is meant to be used with NeutrinoNode")

  private val txDAO = BroadcastAbleTransactionDAO()

  def reset: DataMessageHandler = copy(initialSyncDone = None,
                                       currentFilterBatch = Vector.empty,
                                       filterHeaderHeightOpt = None,
                                       filterHeightOpt = None,
                                       syncPeer = None,
                                       syncing = false,
                                       state = HeaderSync)

  def manager: PeerManager = node.peerManager

  def addToStream(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender,
      peer: Peer): Future[Unit] = {
    val msg = DataMessageWrapper(payload, peerMsgSender, peer)
    manager.dataMessageStream.offer(msg).map(_ => ())
  }

  def handleDataPayload(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender,
      peer: Peer): Future[DataMessageHandler] = {

    lazy val resultF = payload match {
      case checkpoint: CompactFilterCheckPointMessage =>
        logger.debug(
          s"Got ${checkpoint.filterHeaders.size} checkpoints ${checkpoint} from $peer")
        for {
          newChainApi <- chainApi.processCheckpoints(
            checkpoint.filterHeaders.map(_.flip),
            checkpoint.stopHash.flip)
        } yield {
          this.copy(chainApi = newChainApi)
        }
      case filterHeader: CompactFilterHeadersMessage =>
        logger.debug(
          s"Got ${filterHeader.filterHashes.size} compact filter header hashes")
        val filterHeaders = filterHeader.filterHeaders
        for {
          newChainApi <- chainApi.processFilterHeaders(
            filterHeaders,
            filterHeader.stopHash.flip)
          (newSyncing, startFilterHeightOpt) <-
            if (filterHeaders.size == chainConfig.filterHeaderBatchSize) {
              logger.debug(
                s"Received maximum amount of filter headers in one header message. This means we are not synced, requesting more")
              sendNextGetCompactFilterHeadersCommand(
                peerMsgSender,
                filterHeader.stopHash.flip).map(_ => (syncing, None))
            } else {
              for {
                startHeightOpt <- getCompactFilterStartHeight(
                  walletCreationTimeOpt)
                _ = logger.info(
                  s"Done syncing filter headers, beginning to sync filters from startHeightOpt=$startHeightOpt")
                syncing <- sendFirstGetCompactFilterCommand(
                  peerMsgSender,
                  startHeightOpt).map { synced =>
                  if (!synced) logger.info("We are synced")
                  syncing
                }
              } yield (syncing, startHeightOpt)
            }
          newFilterHeaderHeight <- filterHeaderHeightOpt match {
            case None =>
              chainApi.getFilterHeaderCount()
            case Some(filterHeaderHeight) =>
              Future.successful(filterHeaderHeight + filterHeaders.size)
          }
          newChainApi <- newChainApi.setSyncing(newSyncing)
        } yield {
          this.copy(chainApi = newChainApi,
                    syncing = newSyncing,
                    filterHeaderHeightOpt = Some(newFilterHeaderHeight),
                    filterHeightOpt = startFilterHeightOpt)
        }
      case filter: CompactFilterMessage =>
        logger.debug(s"Received ${filter.commandName}, $filter")
        val batchSizeFull: Boolean =
          currentFilterBatch.size == chainConfig.filterBatchSize - 1
        for {
          (newFilterHeaderHeight, newFilterHeight) <-
            calcFilterHeaderFilterHeight()
          newSyncing =
            if (batchSizeFull) {
              syncing
            } else {
              val syncing = newFilterHeight < newFilterHeaderHeight
              if (!syncing) {
                Try(initialSyncDone.map(_.success(Done)))
              }
              syncing
            }
          // If we are not syncing or our filter batch is full, process the filters
          filterBatch = currentFilterBatch :+ filter
          (newBatch, newChainApi) <-
            if (!newSyncing || batchSizeFull) {
              val blockFilters = filterBatch.map { filter =>
                (filter.blockHash,
                 BlockFilter.fromBytes(filter.filterBytes, filter.blockHash))
              }
              logger.info(s"Processing ${filterBatch.size} filters")
              for {
                newChainApi <- chainApi.processFilters(filterBatch)
                _ <-
                  appConfig.callBacks
                    .executeOnCompactFiltersReceivedCallbacks(blockFilters)
              } yield (Vector.empty, newChainApi)
            } else Future.successful((filterBatch, chainApi))
          _ <-
            if (batchSizeFull) {
              logger.info(
                s"Received maximum amount of filters in one batch. This means we are not synced, requesting more")
              sendNextGetCompactFilterCommand(peerMsgSender, newFilterHeight)
            } else Future.unit
          newSyncing2 <- {
            if (!newSyncing) {
              syncIfHeadersAhead(peerMsgSender)
            } else {
              Future.successful(newSyncing)
            }
          }
          newChainApi <- newChainApi.setSyncing(newSyncing2)
          _ <- newChainApi.setIBD(newSyncing)
        } yield {
          this.copy(
            chainApi = newChainApi,
            currentFilterBatch = newBatch,
            syncing = newSyncing2,
            filterHeaderHeightOpt = Some(newFilterHeaderHeight),
            filterHeightOpt = Some(newFilterHeight)
          )
        }
      case notHandling @ (MemPoolMessage | _: GetHeadersMessage |
          _: GetBlocksMessage | _: GetCompactFiltersMessage |
          _: GetCompactFilterHeadersMessage |
          _: GetCompactFilterCheckPointMessage) =>
        logger.debug(s"Received ${notHandling.commandName} message, skipping ")
        Future.successful(this)
      case getData: GetDataMessage =>
        logger.info(
          s"Received a getdata message for inventories=${getData.inventories}")
        getData.inventories.foreach { inv =>
          logger.debug(s"Looking for inv=$inv")
          inv.typeIdentifier match {
            case msgTx @ (TypeIdentifier.MsgTx | TypeIdentifier.MsgWitnessTx) =>
              txDAO.findByHash(inv.hash).flatMap {
                case Some(BroadcastAbleTransaction(tx)) =>
                  val txToBroadcast =
                    if (msgTx == TypeIdentifier.MsgTx) {
                      // send non-witness serialization
                      tx.toBaseTx
                    } else tx // send normal serialization

                  peerMsgSender.sendTransactionMessage(txToBroadcast)
                case None =>
                  logger.warn(
                    s"Got request to send data with hash=${inv.hash}, but found nothing")
                  Future.unit
              }
            case other @ (TypeIdentifier.MsgBlock |
                TypeIdentifier.MsgFilteredBlock |
                TypeIdentifier.MsgCompactBlock |
                TypeIdentifier.MsgFilteredWitnessBlock |
                TypeIdentifier.MsgWitnessBlock) =>
              logger.warn(
                s"Got request to send data type=$other, this is not implemented yet")

            case unassigned: MsgUnassigned =>
              logger.warn(
                s"Received unassigned message we do not understand, msg=${unassigned}")
          }

        }
        Future.successful(this)
      case HeadersMessage(count, headers) =>
        logger.info(
          s"Received headers message with ${count.toInt} headers from $peer")
        logger.trace(
          s"Received headers=${headers.map(_.hashBE.hex).mkString("[", ",", "]")}")
        val chainApiHeaderProcessF: Future[DataMessageHandler] = for {
          newChainApi <- chainApi.setSyncing(count.toInt > 0)
          processed <- newChainApi.processHeaders(headers)
        } yield {
          copy(chainApi = processed)
        }

        val recoveredDMHF: Future[DataMessageHandler] =
          chainApiHeaderProcessF.recoverWith {
            case _: InvalidBlockHeader =>
              logger.debug(
                s"Invalid headers of count $count sent from ${syncPeer.get} in state $state")
              recoverInvalidHeader(peer, peerMsgSender)
            case throwable: Throwable => throw throwable
          }

        val getHeadersF: Future[DataMessageHandler] =
          recoveredDMHF
            .flatMap { newDmh =>
              val newApi = newDmh.chainApi
              if (headers.nonEmpty) {

                val lastHeader = headers.last
                val lastHash = lastHeader.hash
                newApi.getBlockCount().map { count =>
                  logger.trace(
                    s"Processed headers, most recent has height=$count and hash=$lastHash.")
                }

                if (count.toInt == HeadersMessage.MaxHeadersCount) {

                  state match {
                    case HeaderSync =>
                      logger.info(
                        s"Received maximum amount of headers in one header message. This means we are not synced, requesting more")
                      //ask for headers more from the same peer
                      peerMsgSender
                        .sendGetHeadersMessage(lastHash)
                        .map(_ => newDmh)

                    case ValidatingHeaders(inSyncWith, _, _) =>
                      //In the validation stage, some peer sent max amount of valid headers, revert to HeaderSync with that peer as syncPeer
                      //disconnect the ones that we have already checked since they are at least out of sync by 2000 headers
                      val removeFs = inSyncWith.map(p => manager.removePeer(p))

                      val newSyncPeer = Some(peer)

                      //ask for more headers now
                      val askF = peerMsgSender
                        .sendGetHeadersMessage(lastHash)
                        .map(_ => syncing)

                      for {
                        _ <- Future.sequence(removeFs)
                        newSyncing <- askF
                      } yield newDmh.copy(syncing = newSyncing,
                                          state = HeaderSync,
                                          syncPeer = newSyncPeer)

                    case _: DataMessageHandlerState =>
                      Future.successful(newDmh)
                  }

                } else {
                  logger.debug(
                    List(s"Received headers=${count.toInt} in one message,",
                         "which is less than max. This means we are synced,",
                         "not requesting more.")
                      .mkString(" "))
                  // If we are in neutrino mode, we might need to start fetching filters and their headers
                  // if we are syncing we should do this, however, sometimes syncing isn't a good enough check,
                  // so we also check if our cached filter heights have been set as well, if they haven't then
                  // we probably need to sync filters
                  state match {
                    case HeaderSync =>
                      // headers are synced now with the current sync peer, now move to validating it for all peers
                      assert(syncPeer.get == peer)

                      if (manager.peers.size > 1) {
                        val newState =
                          ValidatingHeaders(inSyncWith = Set(peer),
                                            verifyingWith = manager.peers.toSet,
                                            failedCheck = Set.empty[Peer])

                        logger.info(
                          s"Starting to validate headers now. Verifying with ${newState.verifyingWith}")

                        val getHeadersAllF = manager.peerData
                          .filter(_._1 != peer)
                          .map(
                            _._2.peerMessageSender.flatMap(
                              _.sendGetHeadersMessage(lastHash))
                          )

                        Future
                          .sequence(getHeadersAllF)
                          .map(_ => newDmh.copy(state = newState))
                      } else {
                        //if just one peer then can proceed ahead directly
                        fetchCompactFilters(newDmh).map(
                          _.copy(state = PostHeaderSync))
                      }

                    case headerState @ ValidatingHeaders(inSyncWith, _, _) =>
                      //add the current peer to it
                      val newHeaderState =
                        headerState.copy(inSyncWith = inSyncWith + peer)
                      val newDmh2 = newDmh.copy(state = newHeaderState)

                      if (newHeaderState.validated) {
                        // If we are in neutrino mode, we might need to start fetching filters and their headers
                        // if we are syncing we should do this, however, sometimes syncing isn't a good enough check,
                        // so we also check if our cached filter heights have been set as well, if they haven't then
                        // we probably need to sync filters

                        fetchCompactFilters(newDmh2).map(
                          _.copy(state = PostHeaderSync))
                      } else {
                        //do nothing, we are still waiting for some peers to send headers or timeout
                        Future.successful(newDmh2)
                      }

                    case PostHeaderSync =>
                      //send further requests to the same one that sent this
                      if (
                        !syncing ||
                        (filterHeaderHeightOpt.isEmpty &&
                          filterHeightOpt.isEmpty)
                      ) {
                        logger.info(
                          s"Starting to fetch filter headers in data message handler")
                        val newSyncingF =
                          sendFirstGetCompactFilterHeadersCommand(peerMsgSender)
                        newSyncingF.map(newSyncing =>
                          newDmh.copy(syncing = newSyncing))
                      } else {
                        Try(initialSyncDone.map(_.success(Done)))
                        Future.successful(newDmh)
                      }

                  }
                }
              } else {
                //what if we are synced exactly by the 2000th header
                state match {
                  case headerState @ ValidatingHeaders(inSyncWith, _, _) =>
                    val newHeaderState =
                      headerState.copy(inSyncWith = inSyncWith + peer)
                    val newDmh2 = newDmh.copy(state = newHeaderState)

                    if (newHeaderState.validated) {
                      fetchCompactFilters(newDmh2).map(
                        _.copy(state = PostHeaderSync))
                    } else {
                      //do nothing, we are still waiting for some peers to send headers
                      Future.successful(newDmh2)
                    }
                  case _: DataMessageHandlerState =>
                    Future.successful(newDmh)
                }
              }
            }

        getHeadersF.failed.map { err =>
          logger.error(s"Error when processing headers message", err)
        }

        for {
          _ <- chainApiHeaderProcessF
          newDmh <- getHeadersF
          _ <- appConfig.callBacks.executeOnBlockHeadersReceivedCallbacks(
            headers)
        } yield {
          newDmh
        }
      case msg: BlockMessage =>
        val block = msg.block
        logger.info(
          s"Received block message with hash ${block.blockHeader.hash.flip.hex}")

        val newMsgHandlerF = {
          chainApi
            .getHeader(block.blockHeader.hashBE)
            .flatMap { headerOpt =>
              if (headerOpt.isEmpty) {
                logger.debug("Processing block's header...")
                val headersMessage =
                  HeadersMessage(CompactSizeUInt.one, Vector(block.blockHeader))
                for {
                  newMsgHandler <- {
                    // if in IBD, do not process this header, just execute callbacks
                    if (
                      initialSyncDone.isDefined && initialSyncDone.get.isCompleted
                    ) handleDataPayload(headersMessage, peerMsgSender, peer)
                    else {
                      appConfig.callBacks
                        .executeOnBlockHeadersReceivedCallbacks(
                          Vector(block.blockHeader))
                        .map(_ => this)
                    }
                  }
                } yield newMsgHandler
              } else Future.successful(this)
            }
        }

        for {
          handler <- newMsgHandlerF
          _ <-
            appConfig.callBacks
              .executeOnBlockReceivedCallbacks(block)
        } yield {
          handler
        }
      case TransactionMessage(tx) =>
        logger.trace(s"Received txmsg=${tx.txIdBE}, processing given callbacks")
        appConfig.callBacks
          .executeOnTxReceivedCallbacks(tx)
          .map(_ => this)
      case MerkleBlockMessage(_) =>
        logger.warn(s"Merkleblock is not supported")
        Future.successful(this)
      case invMsg: InventoryMessage =>
        handleInventoryMsg(invMsg = invMsg, peerMsgSender = peerMsgSender)
    }

    if (state.isInstanceOf[ValidatingHeaders]) {
      //process messages from all peers
      resultF.failed.foreach { err =>
        logger.error(s"Failed to handle data payload=${payload} from $peer",
                     err)
      }
      resultF.recoverWith { case NonFatal(_) =>
        Future.successful(this)
      }
    } else if (syncPeer.isEmpty || peer != syncPeer.get) {
      //in other states, process messages only from syncPeer
      logger.debug(s"Ignoring ${payload.commandName} from $peer")
      Future.successful(this)
    } else {
      resultF.failed.foreach { err =>
        logger.error(s"Failed to handle data payload=${payload} from $peer",
                     err)
      }
      resultF.recoverWith { case NonFatal(_) =>
        Future.successful(this)
      }

    }
  }

  /** syncs filter headers in case the header chain is still ahead post filter sync */
  def syncIfHeadersAhead(
      peerMessageSender: PeerMessageSender): Future[Boolean] = {
    for {
      headerHeight <- chainApi.getBestHashBlockHeight()
      filterHeaderCount <- chainApi.getFilterHeaderCount()
      filterCount <- chainApi.getFilterCount()
      syncing <- {
        require(headerHeight >= Math.max(filterHeaderCount, filterCount),
                "Header chain cannot be behind filter or filter header chain")
        require(
          filterHeaderCount >= filterCount,
          s"Filter header height $filterHeaderCount must be atleast filter height $filterCount")
        if (headerHeight > filterHeaderCount) {
          logger.info(
            s"Starting to fetch filter headers in data message handler")
          sendFirstGetCompactFilterHeadersCommand(peerMessageSender)
        } else {
          require(
            headerHeight == filterHeaderCount && headerHeight == filterCount)
          logger.info(s"We are synced")
          Try(initialSyncDone.map(_.success(Done)))
          Future.successful(false)
        }
      }
    } yield syncing
  }

  /** Recover the data message handler if we received an invalid block header from a peer */
  private def recoverInvalidHeader(
      peer: Peer,
      peerMsgSender: PeerMessageSender): Future[DataMessageHandler] = {
    state match {
      case HeaderSync =>
        manager.peerData(peer).updateInvalidMessageCount()
        if (
          manager
            .peerData(peer)
            .exceededMaxInvalidMessages && manager.peers.size > 1
        ) {
          logger.info(
            s"$peer exceeded max limit of invalid messages. Disconnecting.")
          for {
            _ <- manager.removePeer(peer)
            newDmh <- manager.syncFromNewPeer()
          } yield newDmh.copy(state = HeaderSync)
        } else {
          logger.info(s"Re-querying headers from $peer.")
          for {
            blockchains <- BlockHeaderDAO().getBlockchains()
            cachedHeaders = blockchains
              .flatMap(_.headers)
              .map(_.hashBE.flip)
            _ <- peerMsgSender.sendGetHeadersMessage(cachedHeaders)
          } yield this
        }

      case headerState @ ValidatingHeaders(_, failedCheck, _) =>
        //if a peer sends invalid data then mark it as failed, dont disconnect
        logger.debug(
          s"Got invalid headers from $peer while validating. Marking as failed.")
        val newHeaderState =
          headerState.copy(failedCheck = failedCheck + peer)
        val newDmh = copy(state = newHeaderState)

        if (newHeaderState.validated) {
          logger.info(
            s"Done validating headers, inSyncWith=${newHeaderState.inSyncWith}, failedCheck=${newHeaderState.failedCheck}")
          fetchCompactFilters(newDmh).map(_.copy(state = PostHeaderSync))
        } else {
          Future.successful(newDmh)
        }

      case _: DataMessageHandlerState =>
        Future.successful(this)
    }
  }

  private def fetchCompactFilters(
      currentDmh: DataMessageHandler): Future[DataMessageHandler] = {
    if (
      !syncing ||
      (filterHeaderHeightOpt.isEmpty &&
        filterHeightOpt.isEmpty)
    ) {
      logger.info(s"Starting to fetch filter headers in data message handler.")

      for {
        peer <- manager.randomPeerWithService(
          ServiceIdentifier.NODE_COMPACT_FILTERS)
        newDmh = currentDmh.copy(syncPeer = Some(peer))
        _ = logger.info(s"Now syncing filters from $peer")
        sender <- manager.peerData(peer).peerMessageSender
        newSyncing <- sendFirstGetCompactFilterHeadersCommand(sender)
      } yield newDmh.copy(syncing = newSyncing)

    } else {
      Try(initialSyncDone.map(_.success(Done)))
      Future.successful(this)
    }
  }

  def onHeaderRequestTimeout(peer: Peer): Future[DataMessageHandler] = {
    logger.info(s"Header request timed out from $peer in state $state")
    state match {
      case HeaderSync =>
        manager.syncFromNewPeer()

      case headerState @ ValidatingHeaders(_, failedCheck, _) =>
        val newHeaderState = headerState.copy(failedCheck = failedCheck + peer)
        val newDmh = copy(state = newHeaderState)

        if (newHeaderState.validated) {
          fetchCompactFilters(newDmh).map(_.copy(state = PostHeaderSync))
        } else Future.successful(newDmh)

      case _: DataMessageHandlerState => Future.successful(this)
    }
  }

  private def sendNextGetCompactFilterHeadersCommand(
      peerMsgSender: PeerMessageSender,
      prevStopHash: DoubleSha256DigestBE): Future[Boolean] =
    peerMsgSender.sendNextGetCompactFilterHeadersCommand(
      chainApi = chainApi,
      filterHeaderBatchSize = chainConfig.filterHeaderBatchSize,
      prevStopHash = prevStopHash)

  private def sendFirstGetCompactFilterHeadersCommand(
      peerMsgSender: PeerMessageSender): Future[Boolean] = {

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
          peerMsgSender
            .sendGetCompactFilterHeadersMessage(filterSyncMarker)
            .map(_ => true)
        case None =>
          sys.error(
            s"Could not find block header in database to sync filter headers from! It's likely your database is corrupted")
      }
    } yield res
  }

  private def sendNextGetCompactFilterCommand(
      peerMsgSender: PeerMessageSender,
      startHeight: Int): Future[Boolean] =
    peerMsgSender.sendNextGetCompactFilterCommand(chainApi = chainApi,
                                                  filterBatchSize =
                                                    chainConfig.filterBatchSize,
                                                  startHeight = startHeight)

  private def sendFirstGetCompactFilterCommand(
      peerMsgSender: PeerMessageSender,
      startHeightOpt: Option[Int]): Future[Boolean] = {
    val startHeightF = startHeightOpt match {
      case Some(startHeight) => Future.successful(startHeight)
      case None              => chainApi.getFilterCount()
    }

    for {
      startHeight <- startHeightF
      res <- sendNextGetCompactFilterCommand(peerMsgSender, startHeight)
    } yield res
  }

  private def handleInventoryMsg(
      invMsg: InventoryMessage,
      peerMsgSender: PeerMessageSender): Future[DataMessageHandler] = {
    logger.debug(s"Received inv=${invMsg}")
    val getData = GetDataMessage(invMsg.inventories.flatMap {
      case Inventory(TypeIdentifier.MsgBlock, hash) =>
        appConfig.nodeType match {
          case NodeType.NeutrinoNode | NodeType.FullNode =>
            if (syncing) None
            else Some(Inventory(TypeIdentifier.MsgWitnessBlock, hash))
          case NodeType.BitcoindBackend =>
            throw new RuntimeException("This is impossible")
        }
      case Inventory(TypeIdentifier.MsgTx, hash) =>
        Some(Inventory(TypeIdentifier.MsgWitnessTx, hash))
      case other: Inventory => Some(other)
    })
    peerMsgSender.sendMsg(getData).map(_ => this)
  }

  private def getCompactFilterStartHeight(
      walletCreationTimeOpt: Option[Instant]): Future[Option[Int]] = {
    walletCreationTimeOpt match {
      case Some(instant) =>
        val creationTimeHeightF = chainApi
          .epochSecondToBlockHeight(instant.toEpochMilli / 1000)
        val filterCountF = chainApi.getFilterCount()
        for {
          creationTimeHeight <- creationTimeHeightF
          filterCount <- filterCountF
        } yield {
          //want to choose the maximum out of these too
          //if our internal chainstate filter count is > creationTimeHeight
          //we just want to start syncing from our last seen filter
          Some(Math.max(creationTimeHeight, filterCount))
        }
      case None =>
        Future.successful(None)
    }
  }

  private def calcFilterHeaderFilterHeight(): Future[(Int, Int)] = {
    (filterHeaderHeightOpt, filterHeightOpt) match {
      case (Some(filterHeaderHeight), Some(filterHeight)) =>
        Future.successful((filterHeaderHeight, filterHeight + 1))
      case (_, _) => // If either are None
        for {
          filterHeaderHeight <- chainApi.getFilterHeaderCount()
          filterHeight <- chainApi.getFilterCount()
        } yield (filterHeaderHeight,
                 if (filterHeight == 0) 0 else filterHeight + 1)
    }
  }
}

sealed trait StreamDataMessageWrapper

case class DataMessageWrapper(
    payload: DataPayload,
    peerMsgSender: PeerMessageSender,
    peer: Peer)
    extends StreamDataMessageWrapper

case class HeaderTimeoutWrapper(peer: Peer) extends StreamDataMessageWrapper
