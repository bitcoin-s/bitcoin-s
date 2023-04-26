package org.bitcoins.node.networking.peer

import org.bitcoins.chain.blockchain.{DuplicateHeaders, InvalidBlockHeader}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.gcs.{BlockFilter, GolombFilter}
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models._
import org.bitcoins.node.networking.peer.DataMessageHandlerState._
import org.bitcoins.node.{P2PLogger, PeerManager}

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}
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
    peerManager: PeerManager,
    state: DataMessageHandlerState,
    filterBatchCache: Set[CompactFilterMessage])(implicit
    ec: ExecutionContext,
    appConfig: NodeAppConfig,
    chainConfig: ChainAppConfig)
    extends P2PLogger {

  require(appConfig.nodeType == NodeType.NeutrinoNode,
          "DataMessageHandler is meant to be used with NeutrinoNode")

  private val txDAO = BroadcastAbleTransactionDAO()

  private val syncing: Boolean = state match {
    case _: HeaderSync | _: ValidatingHeaders | _: FilterHeaderSync |
        _: FilterSync =>
      true
    case _: DoneSyncing => false
  }

  def reset(peer: Peer): DataMessageHandler = {
    copy(filterBatchCache = Set.empty, state = HeaderSync(peer))
  }

  def addToStream(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender,
      peer: Peer): Future[Unit] = {
    val msg = DataMessageWrapper(payload, peerMsgSender, peer)
    peerManager.dataMessageStream.offer(msg).map(_ => ())
  }

  private def isChainIBD: Future[Boolean] = {
    chainApi.isIBD()
  }

  def handleDataPayload(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender,
      peer: Peer): Future[DataMessageHandler] = {
    state match {
      case _: ValidatingHeaders =>
        val resultF = handleDataPayloadValidState(payload, peerMsgSender, peer)
        //process messages from all peers
        resultF.failed.foreach { err =>
          logger.error(
            s"Failed to handle data payload=${payload} from $peer errMsg=${err.getMessage}")
        }
        resultF.recoverWith { case NonFatal(_) =>
          Future.successful(this)
        }
      case state @ (_: HeaderSync | _: FilterHeaderSync | _: FilterSync) =>
        val syncPeer = state.syncPeer
        if (peer != syncPeer) {
          //ignore message from peers that we aren't syncing with during IBD
          logger.warn(
            s"Ignoring message ${payload.commandName} from $peer because we are syncing with this peer currently. syncPeer=$syncPeer")
          Future.successful(this)
        } else {
          val resultF =
            handleDataPayloadValidState(payload, peerMsgSender, peer)
          resultF.failed.foreach { err =>
            logger.error(
              s"Failed to handle data payload=${payload} from $peer errMsg=${err.getMessage}")
          }
          resultF.recoverWith { case NonFatal(_) =>
            Future.successful(this)
          }
        }
      case DoneSyncing(_) =>
        val resultF = handleDataPayloadValidState(payload, peerMsgSender, peer)
        resultF.failed.foreach { err =>
          logger.error(
            s"Failed to handle data payload=${payload} from $peer errMsg=${err.getMessage}")
        }
        resultF.recoverWith { case NonFatal(_) =>
          Future.successful(this)
        }
    }

  }

  /** Processes a [[DataPayload]] if our [[DataMessageHandlerState]] is valid.
    * We ignore messages from certain peers when we are in initial block download.
    */
  private def handleDataPayloadValidState(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender,
      peer: Peer): Future[DataMessageHandler] = {
    payload match {
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
        require(
          state.isInstanceOf[FilterHeaderSync],
          s"Incorrect state for handling filter header messages, got=$state")
        val filterHeaderSync = state.asInstanceOf[FilterHeaderSync]
        val filterHeaders = filterHeader.filterHeaders
        for {
          newChainApi <- chainApi.processFilterHeaders(
            filterHeaders,
            filterHeader.stopHash.flip)
          newState <-
            if (filterHeaders.size == chainConfig.filterHeaderBatchSize) {
              logger.debug(
                s"Received maximum amount of filter headers in one header message. This means we are not synced, requesting more")
              sendNextGetCompactFilterHeadersCommand(
                peerMsgSender,
                filterHeader.stopHash.flip).map(_ => filterHeaderSync)
            } else {
              for {
                startHeightOpt <- getCompactFilterStartHeight(
                  walletCreationTimeOpt)
                syncing <- sendFirstGetCompactFilterCommand(
                  peerMsgSender,
                  startHeightOpt).map { syncing =>
                  if (!syncing)
                    logger.info("Compact filters are already synced")
                  syncing
                }
              } yield {
                if (syncing) {
                  FilterSync(filterHeaderSync.syncPeer)
                } else {
                  DoneSyncing(filterHeaderSync.syncPeer)
                }
              }
            }
          newChainApi <- newChainApi.setSyncing(newState.isSyncing)
        } yield {
          this.copy(chainApi = newChainApi, state = newState)
        }
      case filter: CompactFilterMessage =>
        logger.debug(
          s"Received ${filter.commandName}, filter.blockHash=${filter.blockHash.flip}")
        require(
          state.isInstanceOf[FilterSync],
          s"Can only process filter messages when we are in FilterSync state, got=$state")
        val filterSyncState = state.asInstanceOf[FilterSync]
        val filterBatch = filterBatchCache.+(filter)
        val batchSizeFull: Boolean =
          filterBatch.size == chainConfig.filterBatchSize
        for {
          isFiltersSynced <- isFiltersSynced(chainApi, filterBatch)
          // If we are not syncing or our filter batch is full, process the filters
          (newBatch: Set[CompactFilterMessage], newChainApi) <- {
            if (isFiltersSynced || batchSizeFull) {

              logger.info(s"Processing ${filterBatch.size} filters")
              val sortedBlockFiltersF = sortBlockFiltersByBlockHeight(
                filterBatch)
              for {
                sortedBlockFilters <- sortedBlockFiltersF
                sortedFilterMessages = sortedBlockFilters.map(_._2)
                newChainApi <- chainApi.processFilters(sortedFilterMessages)
                sortedGolombFilters = sortedBlockFilters.map(x => (x._1, x._3))
                _ <-
                  appConfig.callBacks
                    .executeOnCompactFiltersReceivedCallbacks(
                      sortedGolombFilters)
              } yield (Set.empty, newChainApi)
            } else Future.successful((filterBatch, chainApi))
          }
          (_, newFilterHeight) <-
            calcFilterHeaderFilterHeight(chainApi)
          _ <-
            if (batchSizeFull) {
              logger.info(
                s"Received maximum amount of filters in one batch. This means we are not synced, requesting more")
              sendNextGetCompactFilterCommand(peerMsgSender, newFilterHeight)
            } else Future.unit
          newDmhState <- {
            if (isFiltersSynced) {
              syncIfHeadersAhead(peerMsgSender, filterSyncState)
            } else {
              Future.successful(
                DoneSyncing(filterSyncState.syncPeer)
              ) //is this right?
            }
          }
          newChainApi <- newChainApi.setSyncing(newDmhState.isSyncing)
          _ <- checkIBD(newChainApi)
        } yield {
          this.copy(
            chainApi = newChainApi,
            filterBatchCache = newBatch,
            state = newDmhState
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
          s"Received headers message with ${count.toInt} headers from peer=$peer state=$state")
        require(state.isInstanceOf[HeaderSync],
                s"Must receive HeadersMessage in state HeaderSync, got=$state")
        //i think this needs to accept DoneSyncing too as there is a valid state transition from DoneSyncing -> HeaderSyncing?
        val headerSyncState = state.asInstanceOf[HeaderSync]
        val chainApiHeaderProcessF: Future[DataMessageHandler] = for {
          newChainApi <- chainApi.setSyncing(count.toInt > 0)
          processed <- newChainApi.processHeaders(headers)
        } yield {
          copy(chainApi = processed)
        }

        val getHeadersF: Future[DataMessageHandler] = {
          for {
            newDmh <- chainApiHeaderProcessF
            dmh <- getHeaders(newDmh, headers, peerMsgSender, peer, headerSyncState)
          } yield dmh
        }
        getHeadersF.recoverWith {
          case _: DuplicateHeaders =>
            logger.warn(
              s"Received duplicate headers from ${peer} in state=$state")
            Future.successful(this)
          case _: InvalidBlockHeader =>
            logger.warn(
              s"Invalid headers of count $count sent from ${peer} in state=$state")
            recoverInvalidHeader(peerMsgSender)
          case e: Throwable => throw e
        }

        getHeadersF.failed.map { err =>
          logger.error(
            s"Error when processing headers message: ${err.getMessage}")
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
                  isIBD <- isChainIBD
                  newMsgHandler <- {
                    // if in IBD, do not process this header, just execute callbacks
                    if (!isIBD) {
                      handleDataPayload(payload = headersMessage,
                                        peerMsgSender = peerMsgSender,
                                        peer = peer)
                    } else {
                      appConfig.callBacks
                        .executeOnBlockHeadersReceivedCallbacks(
                          Vector(block.blockHeader))
                        .map(_ => this)
                    }
                  }
                } yield {
                  newMsgHandler
                }
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
  }

  /** syncs filter headers in case the header chain is still ahead post filter sync */
  private def syncIfHeadersAhead(
      peerMessageSender: PeerMessageSender,
      filterSyncState: FilterSync): Future[DataMessageHandlerState] = {
    for {
      headerHeight <- chainApi.getBestHashBlockHeight()
      filterHeaderCount <- chainApi.getFilterHeaderCount()
      filterCount <- chainApi.getFilterCount()
      newState <- {
        require(headerHeight >= Math.max(filterHeaderCount, filterCount),
                "Header chain cannot be behind filter or filter header chain")
        require(
          filterHeaderCount >= filterCount,
          s"Filter header height $filterHeaderCount must be atleast filter height $filterCount")
        if (headerHeight > filterHeaderCount) {
          logger.info(
            s"Starting to fetch filter headers in data message handler")
          for {
            syncingFilterHeadersState <- PeerManager
              .sendFirstGetCompactFilterHeadersCommand(peerMessageSender,
                                                       chainApi)
          } yield {
            syncingFilterHeadersState
          }

        } else {
          require(
            headerHeight == filterHeaderCount,
            s"headerHeight=$headerHeight filterHeaderCount=$filterHeaderCount")
          require(headerHeight == filterCount,
                  s"headerHeight=$headerHeight filterCount=$filterCount")
          logger.info(s"We are synced")
          //check to see if we had blocks mined while IBD
          //was ongoing, see: https://github.com/bitcoin-s/bitcoin-s/issues/5036
          for {
            bestBlockHash <- chainApi.getBestBlockHash()
            isIBD <- chainApi.isIBD()
            newState <- {
              if (isIBD) {
                peerMessageSender
                  .sendGetHeadersMessage(bestBlockHash.flip)
                  .map { _ =>
                    HeaderSync(syncPeer = filterSyncState.syncPeer)
                  }
              } else {
                Future.successful(DoneSyncing(filterSyncState.syncPeer))
              }
            }
          } yield newState
        }
      }
    } yield newState
  }

  /** Recover the data message handler if we received an invalid block header from a peer */
  private def recoverInvalidHeader(
      peerMsgSender: PeerMessageSender): Future[DataMessageHandler] = {
    state match {
      case HeaderSync(peer) =>
        peerManager.peerDataMap(peer).updateInvalidMessageCount()
        if (
          peerManager
            .peerDataMap(peer)
            .exceededMaxInvalidMessages && peerManager.peers.size > 1
        ) {
          logger.info(
            s"$peer exceeded max limit of invalid messages. Disconnecting.")
          for {
            _ <- peerManager.removePeer(peer)
            newDmh <- peerManager.syncFromNewPeer()
          } yield newDmh.copy(state = HeaderSync(peer))
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

      case headerState @ ValidatingHeaders(peer, _, failedCheck, _) =>
        //if a peer sends invalid data then mark it as failed, dont disconnect
        logger.warn(
          s"Got invalid headers from peer=$peer while validating. Marking as failed.")
        val newHeaderState =
          headerState.copy(failedCheck = failedCheck + peer)
        val newDmh = copy(state = newHeaderState)

        if (newHeaderState.validated) {
          logger.info(
            s"Done validating headers, inSyncWith=${newHeaderState.inSyncWith}, failedCheck=${newHeaderState.failedCheck}")
          peerManager
            .fetchCompactFilterHeaders(newDmh)
            .map(_.copy(state = DoneSyncing(peer)))
        } else {
          Future.successful(newDmh)
        }
      case _: DoneSyncing =>
        Future.successful(this)
    }
  }

  private def sendNextGetCompactFilterHeadersCommand(
      peerMsgSender: PeerMessageSender,
      prevStopHash: DoubleSha256DigestBE): Future[Boolean] =
    peerMsgSender.sendNextGetCompactFilterHeadersCommand(
      chainApi = chainApi,
      filterHeaderBatchSize = chainConfig.filterHeaderBatchSize,
      prevStopHash = prevStopHash)

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
      _ = logger.info(
        s"Beginning to sync filters from startHeight=$startHeight")
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

  private def calcFilterHeaderFilterHeight(
      chainApi: ChainApi): Future[(Int, Int)] = {
    for {
      filterHeaderHeight <- chainApi.getFilterHeaderCount()
      filterHeight <- chainApi.getFilterCount()
    } yield (filterHeaderHeight, filterHeight)
  }

  /** Helper method to determine if compact filters are synced */
  private def isFiltersSynced(
      chainApi: ChainApi,
      filterBatch: Set[CompactFilterMessage]): Future[Boolean] = {
    for {
      (newFilterHeaderHeight, newFilterHeight) <- calcFilterHeaderFilterHeight(
        chainApi)
      isSynced <-
        if (newFilterHeight == 0 && walletCreationTimeOpt.isDefined) {
          //if we have zero filters in our database and are syncing filters after a wallet creation time
          //we need to calculate the offset of the first filter
          //and how many compact filter headers we have seen. filter_height = best_filter_header - first_filter_filter_header
          val firstFilterBlockHash = filterBatch.head.blockHash.flip
          val filterHeaderOptF = chainApi.getFilterHeader(firstFilterBlockHash)
          val blockCountF = chainApi.getBlockCount()
          for {
            filterHeaderOpt <- filterHeaderOptF
            blockCount <- blockCountF
          } yield {
            filterHeaderOpt match {
              case Some(filterHeader) =>
                (blockCount - filterHeader.height) <= chainConfig.filterBatchSize
              case None =>
                sys.error(
                  s"Could not find filter header associated with blockHash=$firstFilterBlockHash")
            }
          }
        } else if (newFilterHeight == 0 && walletCreationTimeOpt.isEmpty) {
          //fully syncing all filters
          Future.successful(filterBatch.size == newFilterHeaderHeight + 1)
        } else {
          Future.successful(
            (newFilterHeight + filterBatch.size) == newFilterHeaderHeight)
        }
    } yield {
      isSynced
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

  private def sortBlockFiltersByBlockHeight(
      filterBatch: Set[CompactFilterMessage]): Future[
    Vector[(DoubleSha256Digest, CompactFilterMessage, GolombFilter)]] = {
    val blockFiltersF: Future[
      Set[(Int, DoubleSha256Digest, CompactFilterMessage, GolombFilter)]] = {
      Future.traverse(filterBatch) { filter =>
        val blockHeightOptF =
          chainApi.getBlockHeight(filter.blockHash.flip)
        val filtersWithBlockHeightF = for {
          blockHeightOpt <- blockHeightOptF
        } yield {
          require(
            blockHeightOpt.isDefined,
            s"Could not find block height for blockHash=${filter.blockHash.flip}")
          (blockHeightOpt.get,
           filter.blockHash,
           filter,
           BlockFilter.fromBytes(filter.filterBytes, filter.blockHash))
        }

        filtersWithBlockHeightF
      }
    }
    val sortedBlockFiltersF = {
      blockFiltersF
        .map(_.toVector.sortBy(_._1))
        .map(set => set.map(tuple => (tuple._2, tuple._3, tuple._4)))
    }

    sortedBlockFiltersF
  }

  private def getHeaders(
      newDmh: DataMessageHandler,
      headers: Vector[BlockHeader],
      peerMsgSender: PeerMessageSender,
      peer: Peer,
      headerSyncState: HeaderSync): Future[DataMessageHandler] = {
    val count = headers.length
    val getHeadersF: Future[DataMessageHandler] = {
      val newApi = newDmh.chainApi
      if (headers.nonEmpty) {

        val lastHeader = headers.last
        val lastHash = lastHeader.hash
        newApi.getBlockCount().map { count =>
          logger.trace(
            s"Processed headers, most recent has height=$count and hash=$lastHash.")
        }

        if (count == HeadersMessage.MaxHeadersCount) {

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
              val removeFs =
                inSyncWith.map(p => peerManager.removePeer(p))

              val newSyncPeer = Some(peer)

              //ask for more headers now
              val askF = peerMsgSender
                .sendGetHeadersMessage(lastHash)
                .map(_ => syncing)

              for {
                _ <- Future.sequence(removeFs)
                newSyncing <- askF
              } yield {
                val syncPeerOpt = if (newSyncing) {
                  newSyncPeer
                } else {
                  None
                }
                newDmh.copy(state = HeaderSync, syncPeer = syncPeerOpt)
              }

            case _: DataMessageHandlerState =>
              Future.successful(newDmh)
          }

        } else {
          logger.debug(
            List(s"Received headers=${count.toInt} in one message,",
                 "which is less than max. This means we are synced,",
                 s"not requesting more. state=$state")
              .mkString(" "))
          // If we are in neutrino mode, we might need to start fetching filters and their headers
          // if we are syncing we should do this, however, sometimes syncing isn't a good enough check,
          // so we also check if our cached filter heights have been set as well, if they haven't then
          // we probably need to sync filters
          state match {
            case HeaderSync =>
              // headers are synced now with the current sync peer, now move to validating it for all peers
              assert(syncPeer.get == peer)

              if (peerManager.peers.size > 1) {
                val newState =
                  ValidatingHeaders(inSyncWith = Set(peer),
                                    verifyingWith = peerManager.peers.toSet,
                                    failedCheck = Set.empty[Peer])

                logger.info(
                  s"Starting to validate headers now. Verifying with ${newState.verifyingWith}")

                val getHeadersAllF = peerManager.peerDataMap
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
                peerManager
                  .fetchCompactFilterHeaders(newDmh)
                  .map(_.copy(state = PostHeaderSync))
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

                peerManager
                  .fetchCompactFilterHeaders(newDmh2)
                  .map(_.copy(state = PostHeaderSync))
              } else {
                //do nothing, we are still waiting for some peers to send headers or timeout
                Future.successful(newDmh2)
              }

            case PostHeaderSync =>
              //send further requests to the same one that sent this
              logger.info(
                s"Starting to fetch filter headers in data message handler")
              val newSyncingF =
                PeerManager.sendFirstGetCompactFilterHeadersCommand(
                  peerMsgSender,
                  chainApi)
              newSyncingF.map { newSyncing =>
                val syncPeerOpt = if (newSyncing) {
                  syncPeer
                } else {
                  None
                }
                newDmh.copy(syncPeer = syncPeerOpt)
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
              peerManager
                .fetchCompactFilterHeaders(newDmh2)
                .map(_.copy(state = PostHeaderSync))
            } else {
              //do nothing, we are still waiting for some peers to send headers
              Future.successful(newDmh2)
            }
          case HeaderSync | PostHeaderSync =>
            Future.successful(newDmh)
        }
      }
    }
    getHeadersF
  }
}

sealed trait StreamDataMessageWrapper

case class DataMessageWrapper(
    payload: DataPayload,
    peerMsgSender: PeerMessageSender,
    peer: Peer)
    extends StreamDataMessageWrapper

case class HeaderTimeoutWrapper(peer: Peer) extends StreamDataMessageWrapper
