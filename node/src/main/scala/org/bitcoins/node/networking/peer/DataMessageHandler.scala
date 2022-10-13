package org.bitcoins.node.networking.peer

import akka.Done
import org.bitcoins.chain.blockchain.{DuplicateHeaders, InvalidBlockHeader}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.db.CompactFilterHeaderDb
import org.bitcoins.core.api.chain.{ChainApi, FilterHeaderProcessResult}
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.gcs.BlockFilter
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models._
import org.bitcoins.node.networking.peer.DataMessageHandlerState._
import org.bitcoins.node.{Node, P2PLogger, PeerManager}

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Random, Try}

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
        state match {
          case FetchFilterCheckpoints =>
            logger.debug(
              s"Got ${checkpoint.filterHeaders.size} checkpoints ${checkpoint} from $peer")
            for {
              newChainApi <- chainApi.processCheckpoints(
                checkpoint.filterHeaders.map(_.flip),
                checkpoint.stopHash.flip)
              newDmh = copy(chainApi = newChainApi)
              newDmh2 <- fetchFilterHeaders(newDmh)
            } yield {
              newDmh2
            }
          case bad @ (IBDDone | HeaderSync | _: FilterSync |
              _: FilterHeaderSync | _: ValidatingHeaders) =>
            logger.warn(
              s"Unexpected behavior: Got ${checkpoint.filterHeaders.size} checkpoints from $peer in state=$bad")
            Future.successful(this)
        }
      case filterHeader: CompactFilterHeadersMessage =>
        logger.info(
          s"Received ${filterHeader.filterHashes.size} filter header from $peer")
        state match {
          case state: FilterHeaderSync =>
            val filterHeaders = filterHeader.filterHeaders
            val askedForHeight = state.askedFor(peer)
            val newState = state.copy(askedFor = state.askedFor - peer)
            for {
              // processing filter headers
              FilterHeaderProcessResult(newChainApi, startHeight, endHeight) <-
                chainApi
                  .processFilterHeaders(filterHeaders,
                                        filterHeader.stopHash.flip)
              newState3 = (startHeight, endHeight) match {
                case (Some(start), Some(end)) =>
                  //assert start is what we asked for
                  require(
                    start.height == askedForHeight,
                    s"Received invalid filter headers from $peer, startHeight expected=$askedForHeight, got=${start.height}")

                  newState.copy(verifyLater =
                    newState.verifyLater :+ (start, end))
                case (_, _) =>
                  throw new RuntimeException(
                    s"Got filters=$filterHeaders with no start and end height. This should not be possible!")
              }
              // querying for next
              filterSyncMarkerOpt <- chainApi
                .filterSyncMarkerFromHeaderStart(
                  newState3.nextFetchHeight,
                  chainConfig.filterHeaderBatchSize)
              askedMore = filterSyncMarkerOpt.isDefined
              (newSyncing, startFilterHeightOpt, newState4) <-
                if (newState3.failedQueries.nonEmpty) {
                  val (toAsk, newFailedQueries) =
                    newState3.failedQueries.splitAt(1)
                  peerMsgSender
                    .sendMsg(toAsk.head)
                    .map(_ =>
                      (syncing,
                       None,
                       newState3.copy(failedQueries = newFailedQueries)))
                } else if (askedMore) {
                  val nextStart = newState3.nextFetchHeight
                  for {
                    _ <- peerMsgSender
                      .sendGetCompactFilterHeadersMessage(
                        filterSyncMarkerOpt.get)
                  } yield {
                    (syncing,
                     None,
                     newState3.copy(
                       nextFetchHeight =
                         nextStart + chainConfig.filterHeaderBatchSize,
                       askedFor = newState3.askedFor + (peer -> nextStart)))
                  }
                } else if (
                  newState3.askedFor.isEmpty && newState3.failedQueries.isEmpty
                ) {
                  verifyFilterHeaderChainSplit(newState3.verifyLater,
                                               newState3.headerCount)
                  for {
                    startHeightOpt <- getCompactFilterStartHeight(
                      walletCreationTimeOpt)
                    _ = logger.info(
                      s"Done syncing filter headers, beginning to sync filters from startHeightOpt=$startHeightOpt")
                    newState <- fetchFilters(startHeightOpt)
                  } yield (syncing, startHeightOpt, newState)
                } else {
                  Future.successful((syncing, None, newState3))
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
                        filterHeightOpt = startFilterHeightOpt,
                        state = newState4)
            }
          case IBDDone =>
            val filterHeaders = filterHeader.filterHeaders
            for {
              FilterHeaderProcessResult(newChainApi, _, _) <- chainApi
                .processFilterHeaders(filterHeaders, filterHeader.stopHash.flip)
              (newSyncing, startFilterHeightOpt, newState) <-
                if (filterHeaders.size == chainConfig.filterHeaderBatchSize) {
                  throw new RuntimeException(
                    s"Unexpected behavior! Received max filter headers from $peer in state $IBDDone")
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
                  } yield (syncing, startHeightOpt, IBDDone)
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
                        filterHeightOpt = startFilterHeightOpt,
                        state = newState)
            }
          case bad @ (HeaderSync | FetchFilterCheckpoints | _: FilterSync |
              _: ValidatingHeaders) =>
            throw new RuntimeException(
              s"Unexpected behavior! Got filter headers=$filterHeader in state=$bad")
        }
      case filter: CompactFilterMessage =>
        state match {
          case state: FilterSync =>
            logger.debug(s"Received ${filter.commandName}, $filter from $peer")
            val newState = state.copy(askedFor = state.askedFor - peer)
            val batchSizeFull: Boolean =
              currentFilterBatch.size == chainConfig.filterBatchSize - 1
            for {
              (newFilterHeaderHeight, newFilterHeight) <-
                calcFilterHeaderFilterHeight()
              fc <- chainApi.getFilterCount()
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
                     BlockFilter.fromBytes(filter.filterBytes,
                                           filter.blockHash))
                  }
                  logger.debug(
                    s"Processing ${filterBatch.size} filters when in ${state} and ${!newSyncing} , ${batchSizeFull}")
                  for {
                    newChainApi <- chainApi.processFilters(filterBatch)
                    _ <-
                      appConfig.callBacks
                        .executeOnCompactFiltersReceivedCallbacks(blockFilters)
                  } yield (Vector.empty, newChainApi)
                } else {
                  Future.successful((filterBatch, chainApi))
                }
              newState2 <-
                if (newState.failedQueries.nonEmpty) {
                  val (toAsk, newFailedQueries) =
                    newState.failedQueries.splitAt(1)
                  peerMsgSender
                    .sendMsg(toAsk.head)
                    .map(_ => newState.copy(failedQueries = newFailedQueries))
                } else if (batchSizeFull) {
                  logger.info(
                    s"Received maximum amount of filters in one batch. This means we are not synced, requesting more")
                  val nextStart = newState.nextFetchHeight
                  for {
                    filterSyncMakerOpt <- newChainApi
                      .filterSyncMarkerFromHeaderStart(
                        nextStart,
                        chainConfig.filterBatchSize)
                    (askedMore, isFull) <- filterSyncMakerOpt match {
                      case Some(marker) =>
                        peerMsgSender
                          .sendGetCompactFiltersMessage(marker)
                          .map(_ => (true, true))
                      case None =>
                        Future.successful((false, true))
                    }
                    currentTip <- chainApi.getBestHashBlockHeight()
                  } yield {
                    if (askedMore) {
                      if (isFull) {
                        newState.copy(askedFor =
                                        newState.askedFor + (peer -> nextStart),
                                      nextFetchHeight =
                                        nextStart + chainConfig.filterBatchSize)
                      } else {
                        newState.copy(askedFor =
                                        newState.askedFor + (peer -> nextStart),
                                      nextFetchHeight = currentTip + 1)
                      }
                    } else {
                      newState
                    }
                  }
                } else {
                  Future.successful(newState)
                }
              newSyncing2 <- {
                if (!newSyncing) {
                  syncIfHeadersAhead(peerMsgSender)
                } else {
                  Future.successful(newSyncing)
                }
              }
              newChainApi <- newChainApi.setSyncing(newSyncing2)
              _ <- checkIBD(newChainApi)
              newState3 = if (newSyncing) newState2 else IBDDone
            } yield {
              this.copy(
                chainApi = newChainApi,
                currentFilterBatch = newBatch,
                syncing = newSyncing2,
                state = newState3,
                filterHeaderHeightOpt = Some(newFilterHeaderHeight),
                filterHeightOpt = Some(newFilterHeight)
              )
            }
          case IBDDone =>
            logger.debug(s"Received ${filter.commandName}, $filter")
            val batchSizeFull: Boolean =
              currentFilterBatch.size == chainConfig.filterBatchSize - 1
            require(
              !batchSizeFull,
              s"Unexpected behavior! Got a full batch of filters in $IBDDone")
            for {
              (newFilterHeaderHeight, newFilterHeight) <-
                calcFilterHeaderFilterHeight()
              newSyncing = {
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
                     BlockFilter.fromBytes(filter.filterBytes,
                                           filter.blockHash))
                  }
                  logger.info(s"Processing ${filterBatch.size} filters")
                  for {
                    newChainApi <- chainApi.processFilters(filterBatch)
                    _ <-
                      appConfig.callBacks
                        .executeOnCompactFiltersReceivedCallbacks(blockFilters)
                  } yield (Vector.empty, newChainApi)
                } else Future.successful((filterBatch, chainApi))
              newSyncing2 <- {
                if (!newSyncing) {
                  syncIfHeadersAhead(peerMsgSender)
                } else {
                  Future.successful(newSyncing)
                }
              }
              newChainApi <- newChainApi.setSyncing(newSyncing2)
              _ <- checkIBD(newChainApi)
            } yield {
              this.copy(
                chainApi = newChainApi,
                currentFilterBatch = newBatch,
                syncing = newSyncing2,
                filterHeaderHeightOpt = Some(newFilterHeaderHeight),
                filterHeightOpt = Some(newFilterHeight)
              )
            }
          case bad @ (HeaderSync | _: ValidatingHeaders |
              FetchFilterCheckpoints | _: FilterHeaderSync) =>
            throw new RuntimeException(s"Received filter=$filter in state=$bad")
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

        val getHeadersF: Future[DataMessageHandler] =
          chainApiHeaderProcessF
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

                    case bad @ (IBDDone | FetchFilterCheckpoints |
                        _: FilterHeaderSync | _: FilterSync) =>
                      throw new RuntimeException(
                        s"Unexpected behavior. Received max headers from $peer in state=$bad")
                  }
                } else {
                  logger.debug(
                    List(s"Received headers=${count.toInt} in one message,",
                         "which is less than max. This means we are synced,",
                         "not requesting more.")
                      .mkString(" "))
                  state match {
                    case HeaderSync =>
                      checkHeadersWithPeers(peer, newDmh, Some(lastHash))

                    case headerState @ ValidatingHeaders(inSyncWith, _, _) =>
                      //add the current peer to it
                      val newHeaderState =
                        headerState.copy(inSyncWith = inSyncWith + peer)
                      val newDmh2 = newDmh.copy(state = newHeaderState)

                      if (newHeaderState.validated) {
                        fetchFilterCheckpoints(newDmh2).map(
                          _.copy(state = FetchFilterCheckpoints))
                      } else {
                        //do nothing, we are still waiting for some peers to send headers or timeout
                        Future.successful(newDmh2)
                      }

                    case wait @ (FetchFilterCheckpoints | _: ValidatingHeaders |
                        _: FilterSync | _: FilterHeaderSync) =>
                      //currently still in IBD but not in HeaderSync so must be from a new block, don't query for
                      //filter headers right now
                      logger.debug(s"Got $count headers while in state=$wait.")
                      Future.successful(newDmh)
                    case IBDDone =>
                      // If we are in neutrino mode, we might need to start fetching filters and their headers
                      // if we are syncing we should do this, however, sometimes syncing isn't a good enough check,
                      // so we also check if our cached filter heights have been set as well, if they haven't then
                      // we probably need to sync filters
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
                state match {
                  case HeaderSync =>
                    checkHeadersWithPeers(peer, newDmh, lastHashOpt = None)
                  case headerState @ ValidatingHeaders(inSyncWith, _, _) =>
                    val newHeaderState =
                      headerState.copy(inSyncWith = inSyncWith + peer)
                    val newDmh2 = newDmh.copy(state = newHeaderState)

                    if (newHeaderState.validated) {
                      fetchFilterCheckpoints(newDmh2).map(
                        _.copy(state = FetchFilterCheckpoints))
                    } else {
                      //do nothing, we are still waiting for some peers to send headers
                      Future.successful(newDmh2)
                    }
                  case _: DataMessageHandlerState =>
                    Future.successful(newDmh)
                }
              }
            }

        getHeadersF.recoverWith {
          case _: DuplicateHeaders =>
            logger.warn(
              s"Received duplicate headers from ${syncPeer.get} in state=$state")
            Future.successful(this)
          case _: InvalidBlockHeader =>
            logger.warn(
              s"Invalid headers of count $count sent from ${syncPeer.get} in state=$state")
            recoverInvalidHeader(peer, peerMsgSender)
          case e: Throwable => throw e
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

    if (
      state.isInstanceOf[ValidatingHeaders] || state
        .isInstanceOf[FilterHeaderSync] || state.isInstanceOf[FilterSync]
    ) {
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
          fetchFilterCheckpoints(newDmh).map(
            _.copy(state = FetchFilterCheckpoints))
        } else {
          Future.successful(newDmh)
        }

      case _: DataMessageHandlerState =>
        Future.successful(this)
    }
  }

  private def fetchFilterCheckpoints(
      currentDmh: DataMessageHandler): Future[DataMessageHandler] = {
    if (
      !syncing ||
      (filterHeaderHeightOpt.isEmpty &&
        filterHeightOpt.isEmpty)
    ) {
      logger.info(
        s"Starting to fetch filter header checkpoints in data message handler.")

      for {
        peer <- manager.randomPeerWithService(
          ServiceIdentifier.NODE_COMPACT_FILTERS)
        newDmh = currentDmh.copy(syncPeer = Some(peer))
        sender <- manager.peerData(peer).peerMessageSender
        bestHash <- chainApi.getBestBlockHash()
        _ <- sender.sendGetCompactFilterCheckPointMessage(stopHash =
          bestHash.flip)
      } yield newDmh

    } else {
      Try(initialSyncDone.map(_.success(Done)))
      Future.successful(this)
    }
  }

  private def fetchFilterHeaders(
      currentDmh: DataMessageHandler): Future[DataMessageHandler] = {
    if (
      !syncing ||
      (filterHeaderHeightOpt.isEmpty &&
        filterHeightOpt.isEmpty)
    ) {
      logger.info(s"Starting to fetch filter headers in data message handler.")

      for {
        peers <- manager.allPeersWithService(
          ServiceIdentifier.NODE_COMPACT_FILTERS)
        headerCount <- chainApi.getBestHashBlockHeight()
        (nextStartHeight, askedMap) <- FutureUtil
          .foldLeftAsync[(Int, Map[Peer, Int]), Peer]((0, Map.empty), peers)(
            (accum, peer) => {
              val (start, map) = accum
              val senderF = manager.peerData(peer).peerMessageSender
              val res: Future[(Int, Map[Peer, Int])] =
                for {
                  sender <- senderF
                  filterSyncMarkerOpt <- chainApi
                    .filterSyncMarkerFromHeaderStart(
                      start,
                      chainConfig.filterHeaderBatchSize)
                  newAccum <- filterSyncMarkerOpt match {
                    case Some(filterSyncMarker) =>
                      sender
                        .sendGetCompactFilterHeadersMessage(filterSyncMarker)
                        .map { _ =>
                          (start + chainConfig.filterHeaderBatchSize,
                           map + (peer -> start))
                        }
                    case None =>
                      Future.successful((start, map))
                  }
                } yield newAccum

              res
            })
      } yield {
        val newHeaderState = FilterHeaderSync(
          nextFetchHeight = nextStartHeight,
          verifyLater = Vector.empty,
          askedFor = askedMap,
          failedQueries = Vector.empty,
          headerCount = headerCount
        )
        currentDmh.copy(state = newHeaderState, syncing = true)
      }
    } else {
      Try(initialSyncDone.map(_.success(Done)))
      Future.successful(this)
    }
  }

  def fetchFilters(
      startHeightOpt: Option[Int]): Future[DataMessageHandlerState] = {
    for {
      peers <- manager.allPeersWithService(
        ServiceIdentifier.NODE_COMPACT_FILTERS)
      startHeight <- startHeightOpt match {
        case Some(startHeight) => Future.successful(startHeight)
        case None              => chainApi.getFilterCount()
      }
      (nextStartHeight, askedMap) <- FutureUtil
        .foldLeftAsync[(Int, Map[Peer, Int]), Peer]((startHeight, Map.empty),
                                                    peers)((accum, peer) => {
          val (start, map) = accum
          val senderF = manager.peerData(peer).peerMessageSender

          val res: Future[(Int, Map[Peer, Int])] =
            for {
              sender <- senderF
              filterSyncMarkerOpt <- chainApi.filterSyncMarkerFromHeaderStart(
                start,
                chainConfig.filterBatchSize)
              newAccum <- filterSyncMarkerOpt match {
                case Some(filterSyncMarker) =>
                  sender.sendGetCompactFiltersMessage(filterSyncMarker).map {
                    _ =>
                      (start + chainConfig.filterBatchSize,
                       map + (peer -> start))
                  }
                case None =>
                  Future.successful((start, map))
              }
            } yield newAccum

          res
        })
    } yield {
      FilterSync(nextStartHeight, askedMap, failedQueries = Vector.empty)
    }
  }

  private def checkHeadersWithPeers(
      peer: Peer,
      currentDmh: DataMessageHandler,
      lastHashOpt: Option[DoubleSha256Digest]): Future[DataMessageHandler] = {
    // headers are synced now with the current sync peer, now move to validating it for all peers
    assert(syncPeer.get == peer)

    val lastHashF: Future[DoubleSha256Digest] = lastHashOpt match {
      case Some(hash) => Future.successful(hash)
      case None       => chainApi.getBestBlockHash().map(_.flip)
    }

    if (manager.peers.size > 1) {
      val newState =
        ValidatingHeaders(inSyncWith = Set(peer),
                          verifyingWith = manager.peers.toSet,
                          failedCheck = Set.empty[Peer])

      logger.info(
        s"Starting to validate headers now. Verifying with ${newState.verifyingWith}")

      for {
        lastHash <- lastHashF
        peersToCheckWith = manager.peerData
          .filter(_._1 != peer)
        senders <- Future.sequence(peersToCheckWith.map(_._2.peerMessageSender))
        sendFs = senders.map(s => s.sendGetHeadersMessage(lastHash))
        _ <- Future.sequence(sendFs)
      } yield {
        currentDmh.copy(state = newState)
      }
    } else {
      //if just one peer then can proceed ahead directly
      fetchFilterCheckpoints(currentDmh).map(
        _.copy(state = FetchFilterCheckpoints))
    }
  }

  private def onHeaderRequestTimeout(peer: Peer): Future[DataMessageHandler] = {
    logger.info(s"Header request timed out from $peer in state $state")
    state match {
      case HeaderSync =>
        manager.syncFromNewPeer()

      case headerState @ ValidatingHeaders(_, failedCheck, _) =>
        val newHeaderState = headerState.copy(failedCheck = failedCheck + peer)
        val newDmh = copy(state = newHeaderState)

        if (newHeaderState.validated) {
          fetchFilterCheckpoints(newDmh).map(
            _.copy(state = FetchFilterCheckpoints))
        } else Future.successful(newDmh)

      case _: DataMessageHandlerState => Future.successful(this)
    }
  }

  def handleQueryTimeout(msg: DataTimeouts): Future[DataMessageHandler] = {
    msg match {
      case HeaderTimeout(peer)      => onHeaderRequestTimeout(peer)
      case msg: FilterHeaderTimeout => onFilterHeaderRequestTimeout(msg)
      case msg: FilterTimeout       => onFilterRequestTimeout(msg)
    }
  }

  private def onFilterRequestTimeout(
      msg: FilterTimeout): Future[DataMessageHandler] = {
    val payload = msg.msg
    state match {
      case filterState: FilterSync =>
        val availablePeers =
          manager.peers.diff(
            (filterState.askedFor.keys.toVector :+ msg.peer).distinct)
        if (availablePeers.isEmpty) {
          //no other peers
          if (manager.peers.contains(msg.peer)) {
            //we still have the failed peer, ask from it again
            for {
              sender <- manager.peerData(msg.peer).peerMessageSender
              _ <- sender.sendMsg(payload)
            } yield {
              this
            }
          } else {
            //add it to state for next query, remove this from asked list
            val newDmh = copy(state =
              filterState.copy(askedFor = filterState.askedFor - msg.peer,
                               failedQueries =
                                 filterState.failedQueries :+ payload))
            Future.successful(newDmh)
          }
        } else {
          //query from the available peer
          val peer = availablePeers(Random.nextInt(availablePeers.size))
          assert(peer != msg.peer)
          val senderF = manager.peerData(peer).peerMessageSender
          val newState = filterState.copy(askedFor =
            filterState.askedFor + (peer -> payload.startHeight.toInt) - msg.peer)
          for {
            sender <- senderF
            _ <- sender.sendMsg(payload)
          } yield {
            copy(state = newState)
          }
        }
      case IBDDone =>
        for {
          sender <- manager.randomPeerMsgSenderWithService(
            ServiceIdentifier.NODE_COMPACT_FILTERS)
          _ <- sender.sendMsg(payload)
        } yield this
      case state: DataMessageHandlerState =>
        throw new RuntimeException(
          s"Unexpected behavior! Got re-query request for $payload in state=$state")
    }
  }

  private def onFilterHeaderRequestTimeout(
      msg: FilterHeaderTimeout): Future[DataMessageHandler] = {
    val payload = msg.msg
    state match {
      case filterHeaderState: FilterHeaderSync =>
        val availablePeers =
          manager.peers.diff(
            (filterHeaderState.askedFor.keys.toVector :+ msg.peer).distinct)
        if (availablePeers.isEmpty) {
          if (manager.peers.contains(msg.peer)) {
            for {
              sender <- manager.peerData(msg.peer).peerMessageSender
              _ <- sender.sendMsg(payload)
            } yield {
              this
            }
          } else {
            //add it to state for next query
            val newDmh = copy(state = filterHeaderState.copy(
              askedFor = filterHeaderState.askedFor - msg.peer,
              failedQueries = filterHeaderState.failedQueries :+ payload))
            Future.successful(newDmh)
          }
        } else {
          //query from the available peer
          val peer = availablePeers(Random.nextInt(availablePeers.size))
          val senderF = manager.peerData(peer).peerMessageSender
          val newState = filterHeaderState.copy(
            askedFor =
              filterHeaderState.askedFor + (peer -> payload.startHeight.toInt) - msg.peer,
            failedQueries =
              filterHeaderState.failedQueries.filter(_ != payload))
          for {
            sender <- senderF
            _ <- sender.sendMsg(payload)
          } yield {
            copy(state = newState)
          }
        }
      case IBDDone =>
        for {
          sender <- manager.randomPeerMsgSenderWithService(
            ServiceIdentifier.NODE_COMPACT_FILTERS)
          _ <- sender.sendMsg(payload)
        } yield this
      case state: DataMessageHandlerState =>
        throw new RuntimeException(
          s"Unexpected behavior! Got re-query request for $payload in state=$state")
    }
  }

  private def verifyFilterHeaderChainSplit(
      ranges: Vector[(CompactFilterHeaderDb, CompactFilterHeaderDb)],
      expectedHeight: Int): Unit = {
    val sortedRanges = ranges.sortBy(_._1.height)
    if (sortedRanges.isEmpty) {
      throw new IllegalArgumentException("No filter headers received!")
    } else if (sortedRanges.head._1.height != 0) {
      throw new IllegalArgumentException("Starting height must be 0")
    } else if (sortedRanges.last._2.height != expectedHeight) {
      throw new IllegalArgumentException(
        s"Ending height must be $expectedHeight")
    } else {
      sortedRanges.sliding(2).foreach { adjacent =>
        val end = adjacent.head._2
        val start = adjacent.last._1

        require(
          start.previousFilterHeaderBE == end.hashBE && start.height == end.height + 1,
          s"Invalid filter header range! ${adjacent} are not consecutive.")
      }
    }
  }

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
          //filterHeightOpt contains the height of the last filter of the last batch
          //so if we want to start syncing filters from the correct height we need to
          //decrease the computed height
          val height = Math.max(0, creationTimeHeight - 1)
          //want to choose the maximum out of these too
          //if our internal chainstate filter count is > creationTimeHeight
          //we just want to start syncing from our last seen filter
          Some(Math.max(height, filterCount))
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

  /** Checks if the IBD flag needs to be set from true -> false */
  private def checkIBD(chainApi: ChainApi): Future[Unit] = {
    val isSyncingF = chainApi.isSyncing()
    val isIBDF = chainApi.isIBD()
    for {
      isSyncing <- isSyncingF
      isIBD <- isIBDF
      _ <- {
        if (isIBD) {
          chainApi.setIBD(isSyncing)
        } else {
          Future.unit
        }
      }
    } yield ()
  }
}

sealed trait StreamDataMessageWrapper

case class DataMessageWrapper(
    payload: DataPayload,
    peerMsgSender: PeerMessageSender,
    peer: Peer)
    extends StreamDataMessageWrapper

sealed trait DataTimeouts extends StreamDataMessageWrapper {
  def peer: Peer
}

case class HeaderTimeout(peer: Peer) extends DataTimeouts

case class FilterHeaderTimeout(peer: Peer, msg: GetCompactFilterHeadersMessage)
    extends DataTimeouts

case class FilterTimeout(peer: Peer, msg: GetCompactFiltersMessage)
    extends DataTimeouts
