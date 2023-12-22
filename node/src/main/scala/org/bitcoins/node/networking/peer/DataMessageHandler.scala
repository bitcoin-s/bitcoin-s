package org.bitcoins.node.networking.peer

import org.bitcoins.chain.blockchain.{DuplicateHeaders, InvalidBlockHeader}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db.CompactFilterHeaderDb
import org.bitcoins.core.api.node.{NodeState, NodeType, Peer, SyncNodeState}
import org.bitcoins.core.gcs.{BlockFilter, GolombFilter}
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models._
import org.bitcoins.core.api.node.NodeState._
import org.bitcoins.node.util.PeerMessageSenderApi
import org.bitcoins.node.{P2PLogger, PeerManager, PersistentPeerData}

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
    peerMessageSenderApi: PeerMessageSenderApi,
    peerManager: PeerManager,
    state: NodeState)(implicit
    ec: ExecutionContext,
    appConfig: NodeAppConfig,
    chainConfig: ChainAppConfig)
    extends P2PLogger {

  require(appConfig.nodeType == NodeType.NeutrinoNode,
          "DataMessageHandler is meant to be used with NeutrinoNode")

  private val txDAO = BroadcastAbleTransactionDAO()

  private val syncing: Boolean = state.isSyncing

  private def isChainIBD: Future[Boolean] = {
    chainApi.isIBD()
  }

  def handleDataPayload(
      payload: DataPayload,
      peerData: PersistentPeerData): Future[DataMessageHandler] = {
    state match {
      case syncState: SyncNodeState =>
        syncState match {
          case state @ (_: HeaderSync | _: FilterHeaderSync | _: FilterSync) =>
            val syncPeer = state.syncPeer
            if (peerData.peer != syncPeer) {
              //ignore message from peers that we aren't syncing with during IBD
              logger.warn(
                s"Ignoring message ${payload.commandName} from peer=${peerData.peer} in state=$state because we are syncing with this peer currently. syncPeer=$syncPeer")
              Future.successful(this)
            } else {
              val resultF =
                handleDataPayloadValidState(payload, peerData)
              resultF.failed.foreach { err =>
                logger.error(
                  s"Failed to handle data payload=${payload} from peer=${peerData.peer} in state=$state errMsg=${err.getMessage}",
                  err)
              }
              resultF.recoverWith { case NonFatal(_) =>
                Future.successful(this)
              }
            }
        }
      case _: DoneSyncing =>
        val resultF = handleDataPayloadValidState(payload, peerData)

        resultF.failed.foreach { err =>
          logger.error(
            s"Failed to handle data payload=${payload} from peer=${peerData.peer} in state=$state errMsg=${err.getMessage}",
            err)
        }

        resultF.recoverWith { case NonFatal(_) =>
          Future.successful(this)
        }

      case m: MisbehavingPeer =>
        val badPeer = m.badPeer
        val peersWithSvcs = m.peersWithServices
        if (badPeer == peerData.peer) {
          Future.failed(
            new RuntimeException(
              s"Cannot continue processing p2p messages from badPeer=$badPeer"))
        } else {
          //re-review this, we should probably pattern match on old state so we can continue syncing
          //from where we left off?
          val d = DoneSyncing(peersWithSvcs, m.waitingForDisconnection)
          copy(state = d).handleDataPayload(payload, peerData)
        }
      case r: RemovePeers =>
        val badPeers = r.peersToRemove
        val peersWithSvcs = r.peersWithServices
        if (badPeers.exists(_ == peerData.peer)) {
          Future.failed(new RuntimeException(
            s"Cannot continue processing p2p messages from peer we were suppose to remove, peer=${peerData.peer}"))
        } else {
          val d = DoneSyncing(peersWithSvcs, r.waitingForDisconnection)
          copy(state = d).handleDataPayload(payload, peerData)
        }

    }

  }

  /** Processes a [[DataPayload]] if our [[NodeState]] is valid.
    * We ignore messages from certain peers when we are in initial block download.
    */
  private def handleDataPayloadValidState(
      payload: DataPayload,
      peerData: PersistentPeerData): Future[DataMessageHandler] = {
    val peer = peerData.peer
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
            case s @ (_: HeaderSync | _: DoneSyncing) =>
              FilterHeaderSync(peer,
                               s.peersWithServices,
                               s.waitingForDisconnection)
            case filterHeaderSync: FilterHeaderSync => filterHeaderSync
            case x @ (_: FilterSync | _: MisbehavingPeer | _: RemovePeers) =>
              sys.error(
                s"Incorrect state for handling filter header messages, got=$x")
          }

          handleFilterHeadersMessage(filterHeaderSync,
                                     filterHeader,
                                     chainApi,
                                     peer)
            .map(s => copy(state = s))

        case filter: CompactFilterMessage =>
          logger.debug(
            s"Received ${filter.commandName}, filter.blockHash=${filter.blockHash.flip} state=$state")
          val filterSyncState = state match {
            case f: FilterSync => f
            case s @ (_: DoneSyncing | _: FilterHeaderSync) =>
              FilterSync(peer,
                         s.peersWithServices,
                         s.waitingForDisconnection,
                         Set.empty)
            case x @ (_: MisbehavingPeer | _: RemovePeers | _: HeaderSync) =>
              sys.error(s"Incorrect state for handling filter messages, got=$x")
          }
          val filterBatch = filterSyncState.filterBatchCache.+(filter)
          val batchSizeFull: Boolean =
            filterBatch.size == chainConfig.filterBatchSize
          for {
            isFiltersSynced <- isFiltersSynced(chainApi, filterBatch)
            // If we are not syncing or our filter batch is full, process the filters
            (newBatch: Set[CompactFilterMessage], newChainApi) <- {
              if (isFiltersSynced || batchSizeFull) {
                val sortedBlockFiltersF =
                  sortBlockFiltersByBlockHeight(filterBatch)
                for {
                  sortedBlockFilters <- sortedBlockFiltersF
                  sortedFilterMessages = sortedBlockFilters.map(_._2)
                  _ = logger.debug(
                    s"Processing ${filterBatch.size} filters bestBlockHashBE=${sortedFilterMessages.lastOption
                      .map(_.blockHashBE)}")
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
            filterHeaderSyncStateOpt <-
              if (batchSizeFull) {
                logger.debug(
                  s"Received maximum amount of filters in one batch. This means we are not synced, requesting more")
                for {
                  bestBlockHash <- chainApi.getBestBlockHash()
                  fssOpt <- sendNextGetCompactFilterCommand(
                    peerMessageSenderApi = peerMessageSenderApi,
                    startHeightOpt = None,
                    stopBlockHash = bestBlockHash,
                    fs = filterSyncState)
                } yield {
                  fssOpt
                }
              } else Future.successful(Some(filterSyncState))
            newDmhState <- {
              if (isFiltersSynced) {
                syncIfHeadersAhead(filterSyncState)
              } else {
                val res = filterHeaderSyncStateOpt match {
                  case Some(filterSyncState) =>
                    filterSyncState.copy(filterBatchCache = newBatch)
                  case None =>
                    val d = DoneSyncing(filterSyncState.peersWithServices,
                                        filterSyncState.waitingForDisconnection)
                    d
                }
                Future.successful(res)
              }
            }
            newChainApi <- newChainApi.setSyncing(newDmhState.isSyncing)
            _ <- checkIBD(newChainApi)
          } yield {
            this.copy(
              chainApi = newChainApi,
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

                    peerMessageSenderApi.sendTransactionMessage(transaction =
                      txToBroadcast)
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
          val newStateOpt: Option[NodeState] = state match {
            case d: DoneSyncing =>
              val s = if (count.toInt != 0) {
                //why do we sometimes get empty HeadersMessage?
                HeaderSync(peer, d.peersWithServices, d.waitingForDisconnection)
              } else DoneSyncing(d.peersWithServices, d.waitingForDisconnection)
              Some(s)
            case headerSync: HeaderSync =>
              if (headerSync.syncPeer == peer) {
                Some(headerSync)
              } else {
                //means we received a headers message from a peer we aren't syncing with, so ignore for now
                logger.debug(
                  s"Ignoring headers from peer=$peer while we are syncing with syncPeer=${headerSync.syncPeer}")
                None
              }

            case x @ (_: FilterHeaderSync | _: FilterSync) =>
              logger.warn(
                s"Ignoring headers msg with size=${headers.size} while in state=$x from peer=$peer")
              Some(x)
            case x @ (_: MisbehavingPeer | _: RemovePeers) =>
              sys.error(s"Invalid state to receive headers in, got=$x")
          }

          newStateOpt match {
            case Some(h: HeaderSync) =>
              handleHeadersMessage(h, headers, peerData)
                .map(s => copy(state = s))
            case Some(
                  x @ (_: FilterHeaderSync | _: FilterSync | _: DoneSyncing |
                  _: MisbehavingPeer | _: RemovePeers)) =>
              Future.successful(copy(state = x))
            case None =>
              Future.successful(this)
          }
        case msg: BlockMessage =>
          val block = msg.block

          val newMsgHandlerF = for {
            isIBD <- isChainIBD
            headerOpt <- chainApi
              .getHeader(block.blockHeader.hashBE)
            newMsgHandler <- {
              if (isIBD && headerOpt.isEmpty) {
                //ignore block, don't execute callbacks until IBD is done
                logger.info(
                  s"Received block=${block.blockHeader.hashBE.hex} while in IBD, ignoring it until IBD complete state=${state}.")
                Future.successful(this)
              } else if (!isIBD && headerOpt.isEmpty) {
                logger.info(
                  s"Received block=${block.blockHeader.hash.flip.hex}, processing block's header... state=$state")
                val headersMessage =
                  HeadersMessage(CompactSizeUInt.one, Vector(block.blockHeader))
                val newDmhF = handleDataPayload(payload = headersMessage,
                                                peerData = peerData)
                newDmhF.flatMap { dmh =>
                  appConfig.callBacks
                    .executeOnBlockReceivedCallbacks(block)
                    .map(_ => dmh)
                }
              } else {
                logger.info(
                  s"Received block=${block.blockHeader.hash.flip.hex} state=$state isIBD=$isIBD headerOpt=${headerOpt.isDefined}")
                appConfig.callBacks
                  .executeOnBlockReceivedCallbacks(block)
                  .map(_ => this)
              }
            }
          } yield newMsgHandler

          newMsgHandlerF
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
          handleInventoryMsg(invMsg = invMsg)
      }
    }

    wrappedFuture.flatten
  }

  /** syncs filter headers in case the header chain is still ahead post filter sync */
  private def syncIfHeadersAhead(
      syncNodeState: SyncNodeState): Future[NodeState] = {
    val bestBlockHeaderDbF = chainApi.getBestBlockHeader()
    for {
      headerHeight <- chainApi.getBestHashBlockHeight()
      filterHeaderCount <- chainApi.getFilterHeaderCount()
      filterCount <- chainApi.getFilterCount()
      bestBlockHeaderDb <- bestBlockHeaderDbF
      newState <- {
        require(headerHeight >= Math.max(filterHeaderCount, filterCount),
                "Header chain cannot be behind filter or filter header chain")
        require(
          filterHeaderCount >= filterCount,
          s"Filter header height $filterHeaderCount must be atleast filter height $filterCount")
        if (headerHeight > filterHeaderCount) {
          logger.info(
            s"Starting to fetch filter headers in data message handler")
          val fhs = FilterHeaderSync(syncNodeState.syncPeer,
                                     syncNodeState.peersWithServices,
                                     syncNodeState.waitingForDisconnection)

          for {
            syncingFilterHeadersState <- PeerManager
              .sendFirstGetCompactFilterHeadersCommand(
                peerMessageSenderApi = peerMessageSenderApi,
                chainApi = chainApi,
                state = fhs,
                stopBlockHeaderDb = bestBlockHeaderDb)
          } yield {
            syncingFilterHeadersState.getOrElse(
              DoneSyncing(syncNodeState.peersWithServices,
                          syncNodeState.waitingForDisconnection))
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
            d = DoneSyncing(syncNodeState.peersWithServices,
                            syncNodeState.waitingForDisconnection)
            newState <- {
              peerManager
                .gossipGetHeadersMessage(Vector(bestBlockHash))
                .map { _ =>
                  //set to done syncing since we are technically done with IBD
                  //we just need to sync blocks that occurred while we were doing IBD
                  d
                }
            }
          } yield newState
        }
      }
    } yield newState
  }

  /** Recover the data message handler if we received an invalid block header from a peer */
  private def recoverInvalidHeader(
      peerData: PersistentPeerData): Future[NodeState] = {
    val result = state match {
      case state @ (_: HeaderSync | _: DoneSyncing) =>
        val peer = peerData.peer
        peerData.updateInvalidMessageCount()
        if (peerData.exceededMaxInvalidMessages) {
          logger.warn(
            s"$peer exceeded max limit of invalid messages. Disconnecting. peers=${state.peers}")

          val m = MisbehavingPeer(badPeer = peer,
                                  peersWithServices = state.peersWithServices,
                                  waitingForDisconnection =
                                    state.waitingForDisconnection)
          Future.successful(m)
        } else {

          for {
            blockchains <- BlockHeaderDAO().getBlockchains()
            cachedHeaders = blockchains
              .flatMap(_.headers)
              .map(_.hashBE)
            newState <- {
              logger.info(
                s"Received invalid header from peer=$peer. Re-querying headers from peers=${state.peers}. invalidMessages=${peerData.getInvalidMessageCount} peers.size=${state.peers.size}")
              val queryF =
                peerManager.gossipGetHeadersMessage(cachedHeaders)
              //switch to DoneSyncing state until we receive a valid header from our peers
              val d =
                DoneSyncing(state.peersWithServices,
                            state.waitingForDisconnection)
              queryF.map(_ => d)
            }
          } yield newState
        }
      case _: FilterHeaderSync | _: FilterSync =>
        Future.successful(state)
      case m @ (_: MisbehavingPeer | _: RemovePeers) =>
        val exn = new RuntimeException(
          s"Cannot recover invalid headers, got=$m")
        Future.failed(exn)
    }

    result
  }

  private def sendNextGetCompactFilterHeadersCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      syncPeer: Peer,
      prevStopHash: DoubleSha256DigestBE,
      stopHash: DoubleSha256DigestBE): Future[Boolean] =
    PeerManager.sendNextGetCompactFilterHeadersCommand(
      peerMessageSenderApi = peerMessageSenderApi,
      chainApi = chainApi,
      peer = syncPeer,
      filterHeaderBatchSize = chainConfig.filterHeaderBatchSize,
      prevStopHash = prevStopHash,
      stopHash = stopHash
    )

  private def sendNextGetCompactFilterCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      startHeightOpt: Option[Int],
      stopBlockHash: DoubleSha256DigestBE,
      fs: NodeState.FilterSync): Future[Option[NodeState.FilterSync]] = {

    PeerManager
      .sendNextGetCompactFilterCommand(
        peerMessageSenderApi = peerMessageSenderApi,
        chainApi = chainApi,
        filterBatchSize = chainConfig.filterBatchSize,
        startHeightOpt = startHeightOpt,
        stopBlockHash = stopBlockHash,
        peer = fs.syncPeer
      )
      .map { isSyncing =>
        if (isSyncing) {
          val newState = NodeState.FilterSync(
            syncPeer = fs.syncPeer,
            peersWithServices = fs.peersWithServices,
            waitingForDisconnection = fs.waitingForDisconnection,
            filterBatchCache = fs.filterBatchCache)
          Some(newState)
        } else None
      }
  }

  private def sendFirstGetCompactFilterCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      stopBlockHash: DoubleSha256DigestBE,
      startHeight: Int,
      syncNodeState: SyncNodeState): Future[Option[NodeState.FilterSync]] = {
    logger.info(s"Beginning to sync filters to stopBlockHashBE=$stopBlockHash")

    val fs = syncNodeState match {
      case x @ (_: HeaderSync | _: FilterHeaderSync) =>
        FilterSync(x.syncPeer,
                   x.peersWithServices,
                   x.waitingForDisconnection,
                   Set.empty)
      case fs: FilterSync => fs
    }

    sendNextGetCompactFilterCommand(peerMessageSenderApi = peerMessageSenderApi,
                                    startHeightOpt = Some(startHeight),
                                    stopBlockHash = stopBlockHash,
                                    fs = fs)
  }

  private def handleInventoryMsg(
      invMsg: InventoryMessage): Future[DataMessageHandler] = {
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
    peerMessageSenderApi.sendMsg(getData).map(_ => this)
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
    val bestChainTipsF = chainApi.getBestChainTips()

    for {
      (newFilterHeaderHeight, newFilterHeight) <- calcFilterHeaderFilterHeight(
        chainApi)
      isSynced <-
        if (newFilterHeight == 0 && walletCreationTimeOpt.isDefined) {
          //if we have zero filters in our database and are syncing filters after a wallet creation time
          //we need to calculate the offset of the first filter
          //and how many compact filter headers we have seen. filter_height = best_filter_header - first_filter_filter_header
          val bestBlockHashF = chainApi.getBestBlockHash()
          val filterHeadersF: Future[Vector[CompactFilterHeaderDb]] = {
            Future
              .traverse(filterBatch)(f =>
                chainApi.getFilterHeader(f.blockHash.flip))
              .map(_.flatten.toVector)
          }

          for {
            bestBlockHash <- bestBlockHashF
            filterHeaders <- filterHeadersF
          } yield {
            filterHeaders.exists(_.blockHashBE == bestBlockHash)
          }
        } else if (newFilterHeight == 0 && walletCreationTimeOpt.isEmpty) {
          //fully syncing all filters
          Future.successful(filterBatch.size == newFilterHeaderHeight + 1)
        } else {
          for {
            bestChainTips <- bestChainTipsF
          } yield filterBatch.exists(f =>
            bestChainTips.exists(_.hashBE == f.blockHashBE))
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
      state: HeaderSync,
      headers: Vector[BlockHeader],
      peer: Peer,
      chainApi: ChainApi): Future[NodeState] = {
    logger.debug(s"getHeaders() newDmh.state=${state} peer=$peer peers=$peer")
    val count = headers.length
    val getHeadersF: Future[NodeState] = {
      if (headers.nonEmpty) {

        val lastHeader = headers.last
        val lastHash = lastHeader.hash
        chainApi.getBlockCount().map { count =>
          logger.trace(
            s"Processed headers, most recent has height=$count and hash=$lastHash.")
        }

        if (count == HeadersMessage.MaxHeadersCount) {
          logger.debug(
            s"Received maximum amount of headers in one header message. This means we are not synced, requesting more")
          //ask for headers more from the same peer
          peerMessageSenderApi
            .sendGetHeadersMessage(lastHash.flip)
            .map(_ => state)

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
          val syncPeer = state.syncPeer
          // headers are synced now with the current sync peer, now move to validating it for all peers
          require(syncPeer == peer, s"syncPeer=$syncPeer peer=$peer")

          val fhsOptF = {
            for {
              lastBlockHeaderDbOpt <- chainApi.getHeader(lastHash.flip)
              fhs <- PeerManager.fetchCompactFilterHeaders(
                state = state,
                chainApi = chainApi,
                peerMessageSenderApi = peerMessageSenderApi,
                stopBlockHeaderDb = lastBlockHeaderDbOpt.get)
            } yield {
              fhs
            }
          }
          fhsOptF.map {
            case Some(s) => s
            case None    =>
              //is this right? If we don't send cfheaders to our peers, are we done syncing?
              DoneSyncing(state.peersWithServices,
                          state.waitingForDisconnection)
          }
        }
      } else {
        //what if we are synced exactly by the 2000th header
        Future.successful(state)
      }
    }
    getHeadersF
  }

  private def handleHeadersMessage(
      headerSyncState: HeaderSync,
      headers: Vector[BlockHeader],
      peerData: PersistentPeerData): Future[NodeState] = {
    val peer = headerSyncState.syncPeer
    val count = headers.size
    val chainApiHeaderProcessF: Future[DataMessageHandler] = for {
      newChainApi <- chainApi.setSyncing(count > 0)
      processed <- newChainApi.processHeaders(headers)
    } yield {
      copy(state = headerSyncState, chainApi = processed)
    }

    val getHeadersF: Future[NodeState] = {
      for {
        newDmh <- chainApiHeaderProcessF
        dmh <- getHeaders(state = headerSyncState,
                          headers = headers,
                          peer = peer,
                          newDmh.chainApi)
      } yield dmh
    }
    val recoveredStateF: Future[NodeState] = getHeadersF.recoverWith {
      case _: DuplicateHeaders =>
        logger.warn(s"Received duplicate headers from ${peer} in state=$state")
        Future.successful(headerSyncState)
      case _: InvalidBlockHeader =>
        logger.warn(
          s"Invalid headers of count $count sent from ${peer} in state=$state")
        recoverInvalidHeader(peerData)
      case e: Throwable => throw e
    }

    recoveredStateF.failed.map { err =>
      logger.error(s"Error when processing headers message: ${err.getMessage}")
    }

    for {
      newState <- recoveredStateF
      _ <- {
        if (count == 0) {
          Future.unit //don't execute callbacks if we receive 0 headers from peer
        } else {
          appConfig.callBacks.executeOnBlockHeadersReceivedCallbacks(headers)
        }

      }
    } yield {
      newState
    }
  }

  private def handleFilterHeadersMessage(
      filterHeaderSync: FilterHeaderSync,
      filterHeader: CompactFilterHeadersMessage,
      chainApi: ChainApi,
      peer: Peer): Future[NodeState] = {
    val filterHeaders = filterHeader.filterHeaders
    val blockCountF = chainApi.getBlockCount()
    val bestBlockHashF = chainApi.getBestBlockHash()
    for {
      _ <- chainApi.processFilterHeaders(filterHeaders,
                                         filterHeader.stopHash.flip)
      filterHeaderCount <- chainApi.getFilterHeaderCount()
      blockCount <- blockCountF
      bestBlockHash <- bestBlockHashF
      newState <-
        if (blockCount != filterHeaderCount) {
          logger.debug(
            s"Received maximum amount of filter headers in one header message. This means we are not synced, requesting more")
          sendNextGetCompactFilterHeadersCommand(
            peerMessageSenderApi = peerMessageSenderApi,
            syncPeer = peer,
            prevStopHash = filterHeader.stopHash.flip,
            stopHash = bestBlockHash).map(_ => filterHeaderSync)
        } else {
          for {
            startHeight <- PeerManager.getCompactFilterStartHeight(
              chainApi,
              walletCreationTimeOpt)
            bestBlockHash <- bestBlockHashF
            filterSyncStateOpt <- sendFirstGetCompactFilterCommand(
              peerMessageSenderApi = peerMessageSenderApi,
              stopBlockHash = bestBlockHash,
              startHeight = startHeight,
              syncNodeState = filterHeaderSync)
          } yield {
            filterSyncStateOpt match {
              case Some(filterSyncState) => filterSyncState
              case None =>
                val d =
                  DoneSyncing(filterHeaderSync.peersWithServices,
                              filterHeaderSync.waitingForDisconnection)
                d
            }
          }
        }
      _ <- chainApi.setSyncing(newState.isSyncing)
    } yield {
      newState
    }
  }

}
