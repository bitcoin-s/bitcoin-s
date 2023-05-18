package org.bitcoins.node.networking.peer

import akka.stream.scaladsl.SourceQueue
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
import org.bitcoins.node.util.PeerMessageSenderApi
import org.bitcoins.node.{P2PLogger, PeerData, PeerManager}

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
    queue: SourceQueue[StreamDataMessageWrapper],
    peers: Vector[Peer],
    peerMessgeSenderApi: PeerMessageSenderApi,
    peerDataOpt: Option[PeerData],
    state: DataMessageHandlerState,
    filterBatchCache: Set[CompactFilterMessage])(implicit
    ec: ExecutionContext,
    appConfig: NodeAppConfig,
    chainConfig: ChainAppConfig)
    extends P2PLogger {

  require(appConfig.nodeType == NodeType.NeutrinoNode,
          "DataMessageHandler is meant to be used with NeutrinoNode")

  private val txDAO = BroadcastAbleTransactionDAO()

  private val syncing: Boolean = state.isSyncing

  def reset: DataMessageHandler = {
    copy(filterBatchCache = Set.empty, state = DoneSyncing)
  }

  def addToStream(payload: DataPayload, peer: Peer): Future[Unit] = {
    val msg = DataMessageWrapper(payload, peer)
    queue
      .offer(msg)
      .map(_ => ())
  }

  private def isChainIBD: Future[Boolean] = {
    chainApi.isIBD()
  }

  def handleDataPayload(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender,
      peer: Peer): Future[DataMessageHandler] = {
    state match {
      case syncState: SyncDataMessageHandlerState =>
        syncState match {
          case _: ValidatingHeaders =>
            val resultF =
              handleDataPayloadValidState(payload, peerMsgSender, peer)
            //process messages from all peers
            resultF.failed.foreach { err =>
              logger.error(
                s"Failed to handle data payload=${payload} from $peer in state=$state errMsg=${err.getMessage}",
                err)
            }
            resultF.recoverWith { case NonFatal(_) =>
              Future.successful(this)
            }
          case state @ (_: HeaderSync | _: FilterHeaderSync | _: FilterSync) =>
            val syncPeer = state.syncPeer
            if (peer != syncPeer) {
              //ignore message from peers that we aren't syncing with during IBD
              logger.warn(
                s"Ignoring message ${payload.commandName} from $peer in state=$state because we are syncing with this peer currently. syncPeer=$syncPeer")
              Future.successful(this)
            } else {
              val resultF =
                handleDataPayloadValidState(payload, peerMsgSender, peer)
              resultF.failed.foreach { err =>
                logger.error(
                  s"Failed to handle data payload=${payload} from $peer in state=$state errMsg=${err.getMessage}",
                  err)
              }
              resultF.recoverWith { case NonFatal(_) =>
                Future.successful(this)
              }
            }
        }
      case DoneSyncing =>
        val resultF = handleDataPayloadValidState(payload, peerMsgSender, peer)

        resultF.failed.foreach { err =>
          logger.error(
            s"Failed to handle data payload=${payload} from $peer in state=$state errMsg=${err.getMessage}",
            err)
        }

        resultF.recoverWith { case NonFatal(_) =>
          Future.successful(this)
        }

      case MisbehavingPeer(badPeer) =>
        if (badPeer == peer) {
          Future.failed(
            new RuntimeException(
              s"Cannot continue processing p2p messages from badPeer=$badPeer"))
        } else {
          //re-review this, we should probably pattern match on old state so we can continue syncing
          //from where we left off?
          copy(state = DoneSyncing).handleDataPayload(payload,
                                                      peerMsgSender,
                                                      peer)
        }
      case RemovePeers(peers, _) =>
        if (peers.exists(_ == peer)) {
          Future.failed(new RuntimeException(
            s"Cannot continue processing p2p messages from peer we were suppose to remove, peer=$peer"))
        } else {
          copy(state = DoneSyncing).handleDataPayload(payload,
                                                      peerMsgSender,
                                                      peer)
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

    val wrappedFuture: Future[Future[DataMessageHandler]] = Future {
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
            s"Got ${filterHeader.filterHashes.size} compact filter header hashes, state=$state")
          val filterHeaderSync = state match {
            //is validating headers right here? Re-review these state transitions
            case _: HeaderSync | _: ValidatingHeaders =>
              FilterHeaderSync(peer)
            case filterHeaderSync: FilterHeaderSync => filterHeaderSync
            case x @ (DoneSyncing | _: FilterSync | _: MisbehavingPeer |
                _: RemovePeers) =>
              sys.error(
                s"Incorrect state for handling filter header messages, got=$x")
          }
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
                  peerManager = peerManager,
                  syncPeer = peer,
                  prevStopHash = filterHeader.stopHash.flip).map(_ =>
                  filterHeaderSync)
              } else {
                for {
                  startHeightOpt <- getCompactFilterStartHeight(
                    walletCreationTimeOpt)
                  filterSyncStateOpt <- sendFirstGetCompactFilterCommand(
                    peerManager = peerManager,
                    syncPeer = peer,
                    startHeightOpt = startHeightOpt)
                } yield {
                  filterSyncStateOpt match {
                    case Some(filterSyncState) => filterSyncState
                    case None                  => DoneSyncing
                  }
                }
              }
            newChainApi <- newChainApi.setSyncing(newState.isSyncing)
          } yield {
            this.copy(chainApi = newChainApi, state = newState)
          }
        case filter: CompactFilterMessage =>
          logger.debug(
            s"Received ${filter.commandName}, filter.blockHash=${filter.blockHash.flip} state=$state")
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
                val sortedBlockFiltersF =
                  sortBlockFiltersByBlockHeight(filterBatch)
                for {
                  sortedBlockFilters <- sortedBlockFiltersF
                  sortedFilterMessages = sortedBlockFilters.map(_._2)
                  newChainApi <- chainApi.processFilters(sortedFilterMessages)
                  sortedGolombFilters = sortedBlockFilters.map(x =>
                    (x._1, x._3))
                  _ <-
                    appConfig.callBacks
                      .executeOnCompactFiltersReceivedCallbacks(
                        sortedGolombFilters)
                } yield (Set.empty, newChainApi)
              } else Future.successful((filterBatch, chainApi))
            }
            (_, newFilterHeight) <-
              calcFilterHeaderFilterHeight(chainApi)
            filterHeaderSyncStateOpt <-
              if (batchSizeFull) {
                logger.info(
                  s"Received maximum amount of filters in one batch. This means we are not synced, requesting more")
                sendNextGetCompactFilterCommand(peerManager,
                                                peer,
                                                newFilterHeight)
              } else Future.successful(Some(filterSyncState))
            newDmhState <- {
              if (isFiltersSynced) {
                syncIfHeadersAhead(peer)
              } else {
                val res = filterHeaderSyncStateOpt match {
                  case Some(filterSyncState) => filterSyncState
                  case None                  => DoneSyncing
                }
                Future.successful(res)
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
          logger.debug(
            s"Received ${notHandling.commandName} message, skipping ")
          Future.successful(this)
        case getData: GetDataMessage =>
          logger.info(
            s"Received a getdata message for inventories=${getData.inventories}")
          getData.inventories.foreach { inv =>
            logger.debug(s"Looking for inv=$inv")
            inv.typeIdentifier match {
              case msgTx @ (TypeIdentifier.MsgTx |
                  TypeIdentifier.MsgWitnessTx) =>
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
          val headerSyncState = state match {
            case DoneSyncing =>
              if (count.toInt != 0) {
                //why do we sometimes get empty HeadersMessage?
                HeaderSync(peer)
              } else DoneSyncing
            case headerSync: HeaderSync =>
              if (headerSync.syncPeer == peer) {
                headerSync
              } else {
                //are these Set.empty correct?
                ValidatingHeaders(syncPeer = peer,
                                  inSyncWith = Set.empty,
                                  failedCheck = Set.empty,
                                  verifyingWith = peers.toSet)
              }
            case v: ValidatingHeaders => v
            case x @ (_: FilterHeaderSync | _: FilterSync | _: MisbehavingPeer |
                _: RemovePeers) =>
              sys.error(s"Invalid state to receive headers in, got=$x")
          }
          val chainApiHeaderProcessF: Future[DataMessageHandler] = for {
            newChainApi <- chainApi.setSyncing(count.toInt > 0)
            processed <- newChainApi.processHeaders(headers)
          } yield {
            copy(state = headerSyncState, chainApi = processed)
          }

          val getHeadersF: Future[DataMessageHandler] = {
            for {
              newDmh <- chainApiHeaderProcessF
              dmh <- getHeaders(newDmh = newDmh,
                                headers = headers,
                                peerMsgSender = peerMsgSender,
                                peer = peer,
                                peerDataOpt = peerDataOpt)
            } yield dmh
          }
          val recoveredDmhF = getHeadersF.recoverWith {
            case _: DuplicateHeaders =>
              logger.warn(
                s"Received duplicate headers from ${peer} in state=$state")
              Future.successful(this)
            case _: InvalidBlockHeader =>
              logger.warn(
                s"Invalid headers of count $count sent from ${peer} in state=$state")
              recoverInvalidHeader(peerDataOpt.get)
            case e: Throwable => throw e
          }

          recoveredDmhF.failed.map { err =>
            logger.error(
              s"Error when processing headers message: ${err.getMessage}")
          }

          for {
            newDmh <- recoveredDmhF
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
                    HeadersMessage(CompactSizeUInt.one,
                                   Vector(block.blockHeader))
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
          logger.trace(
            s"Received txmsg=${tx.txIdBE}, processing given callbacks")
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

    wrappedFuture.flatten
  }

  /** syncs filter headers in case the header chain is still ahead post filter sync */
  private def syncIfHeadersAhead(
      syncPeer: Peer): Future[DataMessageHandlerState] = {
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
              .sendFirstGetCompactFilterHeadersCommand(peerManager =
                                                         peerManager,
                                                       chainApi = chainApi,
                                                       peer = syncPeer)
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
                peerManager
                  .sendGetHeadersMessage(Vector(bestBlockHash), Some(syncPeer))
                  .map { _ =>
                    //set to done syncing since we are technically done with IBD
                    //we just need to sync blocks that occurred while we were doing IBD
                    DoneSyncing
                  }
              } else {
                Future.successful(DoneSyncing)
              }
            }
          } yield newState
        }
      }
    } yield newState
  }

  /** Recover the data message handler if we received an invalid block header from a peer */
  private def recoverInvalidHeader(peerData: PeerData): Future[DataMessageHandler] = {
    val result = state match {
      case HeaderSync(peer) =>
        peerData.updateInvalidMessageCount()
        if (peerData.exceededMaxInvalidMessages && peers.size > 1) {
          logger.warn(
            s"$peer exceeded max limit of invalid messages. Disconnecting.")

          Future.successful(copy(state = MisbehavingPeer(peer)))
        } else {
          logger.info(
            s"Re-querying headers from $peer. invalidMessages=${peerData.getInvalidMessageCount} peers.size=${peers.size}")
          for {
            blockchains <- BlockHeaderDAO().getBlockchains()
            cachedHeaders = blockchains
              .flatMap(_.headers)
              .map(_.hashBE)
            _ <- peerManager.sendGetHeadersMessage(cachedHeaders, Some(peer))
          } yield this.copy(state = HeaderSync(peer))
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
          fetchCompactFilterHeaders(newDmh, peerData)
        } else {
          Future.successful(newDmh)
        }
      case DoneSyncing | _: FilterHeaderSync | _: FilterSync =>
        Future.successful(this)
      case m @ (_: MisbehavingPeer | _: RemovePeers) =>
        sys.error(s"Cannot recover invalid headers, got=$m")
    }

    result
  }

  private def sendNextGetCompactFilterHeadersCommand(
      peerManager: PeerManager,
      syncPeer: Peer,
      prevStopHash: DoubleSha256DigestBE): Future[Boolean] =
    peerManager.sendNextGetCompactFilterHeadersCommand(
      chainApi = chainApi,
      peer = syncPeer,
      filterHeaderBatchSize = chainConfig.filterHeaderBatchSize,
      prevStopHash = prevStopHash)

  private def sendNextGetCompactFilterCommand(
      peerManager: PeerManager,
      syncPeer: Peer,
      startHeight: Int): Future[Option[DataMessageHandlerState.FilterSync]] = {

    peerManager
      .sendNextGetCompactFilterCommand(chainApi = chainApi,
                                       filterBatchSize =
                                         chainConfig.filterBatchSize,
                                       startHeight = startHeight,
                                       syncPeer)
      .map { isSyncing =>
        if (isSyncing)
          Some(DataMessageHandlerState.FilterSync(syncPeer))
        else None
      }
  }

  private def sendFirstGetCompactFilterCommand(
      peerManager: PeerManager,
      syncPeer: Peer,
      startHeightOpt: Option[Int]): Future[
    Option[DataMessageHandlerState.FilterSync]] = {
    val startHeightF = startHeightOpt match {
      case Some(startHeight) => Future.successful(startHeight)
      case None              => chainApi.getFilterCount()
    }

    for {
      startHeight <- startHeightF
      _ = logger.info(
        s"Beginning to sync filters from startHeight=$startHeight")
      res <- sendNextGetCompactFilterCommand(peerManager, syncPeer, startHeight)
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
            if (syncing) {
              logger.info(s"Ignoring inv message=$invMsg while in state=$state")
              None
            } else Some(Inventory(TypeIdentifier.MsgWitnessBlock, hash))
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
      peerDataOpt: Option[PeerData]): Future[DataMessageHandler] = {
    val state = newDmh.state
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
            case HeaderSync(_) =>
              logger.info(
                s"Received maximum amount of headers in one header message. This means we are not synced, requesting more")
              //ask for headers more from the same peer
              peerManager
                .sendGetHeadersMessage(lastHash.flip, Some(peer))
                .map(_ => newDmh)

            case ValidatingHeaders(_, inSyncWith, _, _) =>
              //ask for more headers now
              val askF = peerManager
                .sendGetHeadersMessage(lastHash.flip, Some(peer))
                .map(_ => syncing)

              for {
                _ <- askF
              } yield {
                newDmh.copy(state = RemovePeers(inSyncWith.toVector, true))
              }
            case x @ (_: FilterHeaderSync | _: FilterSync | DoneSyncing |
                _: MisbehavingPeer | _: RemovePeers) =>
              sys.error(s"Cannot be in state=$x while retrieving block headers")
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
            case HeaderSync(syncPeer) =>
              // headers are synced now with the current sync peer, now move to validating it for all peers
              require(syncPeer == peer, s"syncPeer=$syncPeer peer=$peer")

              if (peers.size > 1) {
                val newState =
                  ValidatingHeaders(
                    syncPeer = peer,
                    inSyncWith = Set(peer),
                    verifyingWith = peers.toSet,
                    failedCheck = Set.empty[Peer]
                  )

                logger.info(
                  s"Starting to validate headers now. Verifying with ${newState.verifyingWith}")

                val getHeadersAllF = {
                  val msg = GetHeadersMessage(lastHash)
                  peerMessgeSenderApi.gossipMessage(msg,
                                                    excludedPeerOpt =
                                                      Some(peer))
                }

                getHeadersAllF
                  .map(_ => newDmh.copy(state = newState))
              } else {
                //if just one peer then can proceed ahead directly
                fetchCompactFilterHeaders(newDmh, peerDataOpt.get)
              }

            case headerState @ ValidatingHeaders(_, inSyncWith, _, _) =>
              //add the current peer to it
              val newHeaderState =
                headerState.copy(syncPeer = peer,
                                 inSyncWith = inSyncWith + peer)
              val newDmh2 = newDmh.copy(state = newHeaderState)

              if (newHeaderState.validated) {
                // If we are in neutrino mode, we might need to start fetching filters and their headers
                // if we are syncing we should do this, however, sometimes syncing isn't a good enough check,
                // so we also check if our cached filter heights have been set as well, if they haven't then
                // we probably need to sync filters

                fetchCompactFilterHeaders(currentDmh = newDmh2,
                                          peerData = peerDataOpt.get)
              } else {
                //do nothing, we are still waiting for some peers to send headers or timeout
                Future.successful(newDmh2)
              }
            case x @ (_: FilterHeaderSync | _: FilterSync | DoneSyncing |
                _: MisbehavingPeer | _: RemovePeers) =>
              sys.error(
                s"Cannot be in state=$x while we are about to begin syncing compact filter headers")
          }
        }
      } else {
        //what if we are synced exactly by the 2000th header
        state match {
          case headerState @ ValidatingHeaders(_, inSyncWith, _, _) =>
            val newHeaderState =
              headerState.copy(inSyncWith = inSyncWith + peer)
            val newDmh2 = newDmh.copy(state = newHeaderState)
            if (newHeaderState.validated) {
              fetchCompactFilterHeaders(newDmh2, peerDataOpt.get)
            } else {
              //do nothing, we are still waiting for some peers to send headers
              Future.successful(newDmh2)
            }
          case _: HeaderSync =>
            Future.successful(newDmh)
          case DoneSyncing =>
            Future.successful(newDmh)
          case x @ (_: FilterHeaderSync | _: FilterSync | _: MisbehavingPeer |
              _: RemovePeers) =>
            sys.error(s"Invalid state to complete block header sync in, got=$x")
        }
      }
    }
    getHeadersF
  }

  def fetchCompactFilterHeaders(
      currentDmh: DataMessageHandler,
      peerData: PeerData): Future[DataMessageHandler] = {
    val syncPeer = currentDmh.state match {
      case s: SyncDataMessageHandlerState => s.syncPeer
      case state @ (DoneSyncing | _: MisbehavingPeer | _: RemovePeers) =>
        sys.error(
          s"Cannot fetch compact filter headers when we are in state=$state")
    }
    logger.info(
      s"Now syncing filter headers from $syncPeer in state=${currentDmh.state}")
    for {
      sender <- peerData.peerMessageSender
      newSyncingState <- PeerManager.sendFirstGetCompactFilterHeadersCommand(
        sender,
        currentDmh.chainApi)
    } yield {
      currentDmh.copy(state = newSyncingState)
    }
  }
}

sealed trait StreamDataMessageWrapper {
  def peer: Peer
}

case class DataMessageWrapper(payload: DataPayload, peer: Peer)
    extends StreamDataMessageWrapper

case class HeaderTimeoutWrapper(peer: Peer) extends StreamDataMessageWrapper

case class DisconnectedPeer(peer: Peer, forceReconnect: Boolean)
    extends StreamDataMessageWrapper

case class Initialized(peer: Peer) extends StreamDataMessageWrapper

case class InitializationTimeout(peer: Peer) extends StreamDataMessageWrapper

case class QueryTimeout(peer: Peer, payload: ExpectsResponse)
    extends StreamDataMessageWrapper

case class SendResponseTimeout(peer: Peer, payload: NetworkPayload)
case class SendToPeer(msg: NetworkMessage, peerOpt: Option[Peer])
    extends StreamDataMessageWrapper
