package org.bitcoins.node.networking.peer

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.db.ChainApi
import org.bitcoins.core.gcs.BlockFilter
import org.bitcoins.core.p2p._
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.BroadcastAbleTransactionDAO
import org.bitcoins.node.{NodeCallbacks, NodeType, P2PLogger}

import scala.concurrent.{ExecutionContext, Future}

/** This actor is meant to handle a [[org.bitcoins.core.p2p.DataPayload DataPayload]]
  * that a peer to sent to us on the p2p network, for instance, if we a receive a
  * [[org.bitcoins.core.p2p.HeadersMessage HeadersMessage]] we should store those headers in our database
  *
  * @param currentFilterBatch holds the current batch of filters to be processed, after its size reaches
  *                           chainConfig.filterBatchSize they will be processed and then emptied
  */
case class DataMessageHandler(
    chainApi: ChainApi,
    callbacks: NodeCallbacks,
    currentFilterBatch: Vector[CompactFilterMessage] = Vector.empty,
    filterHeaderHeightOpt: Option[Int] = None,
    filterHeightOpt: Option[Int] = None,
    syncing: Boolean = false)(implicit
    ec: ExecutionContext,
    appConfig: NodeAppConfig,
    chainConfig: ChainAppConfig)
    extends P2PLogger {

  private val txDAO = BroadcastAbleTransactionDAO()

  def handleDataPayload(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender): Future[DataMessageHandler] = {

    payload match {
      case checkpoint: CompactFilterCheckPointMessage =>
        logger.debug(
          s"Got ${checkpoint.filterHeaders.size} checkpoints ${checkpoint}")
        for {
          newChainApi <- chainApi.processCheckpoints(
            checkpoint.filterHeaders.map(_.flip),
            checkpoint.stopHash.flip)
        } yield {
          this.copy(chainApi = newChainApi)
        }
      case filterHeader: CompactFilterHeadersMessage =>
        logger.info(
          s"Got ${filterHeader.filterHashes.size} compact filter header hashes")
        val filterHeaders = filterHeader.filterHeaders
        for {
          newChainApi <- chainApi.processFilterHeaders(
            filterHeaders,
            filterHeader.stopHash.flip)
          newSyncing <-
            if (filterHeaders.size == chainConfig.filterHeaderBatchSize) {
              logger.info(
                s"Received maximum amount of filter headers in one header message. This means we are not synced, requesting more")
              sendNextGetCompactFilterHeadersCommand(
                peerMsgSender,
                filterHeader.stopHash.flip).map(_ => syncing)
            } else {
              logger.debug(
                s"Received filter headers=${filterHeaders.size} in one message, " +
                  "which is less than max. This means we are synced.")
              sendFirstGetCompactFilterCommand(peerMsgSender).map { synced =>
                if (!synced)
                  logger.info("We are synced")
                synced
              }
            }
          newFilterHeaderHeight <- filterHeaderHeightOpt match {
            case None =>
              chainApi.getFilterHeaderCount()
            case Some(filterHeaderHeight) =>
              Future.successful(filterHeaderHeight + filterHeaders.size)
          }
        } yield {
          this.copy(chainApi = newChainApi,
                    syncing = newSyncing,
                    filterHeaderHeightOpt = Some(newFilterHeaderHeight))
        }
      case filter: CompactFilterMessage =>
        logger.debug(s"Received ${filter.commandName}, $filter")
        val batchSizeFull: Boolean =
          currentFilterBatch.size == chainConfig.filterBatchSize - 1
        for {
          (newFilterHeaderHeight, newFilterHeight) <-
            (filterHeaderHeightOpt, filterHeightOpt) match {
              case (Some(filterHeaderHeight), Some(filterHeight)) =>
                Future.successful((filterHeaderHeight, filterHeight + 1))
              case (_, _) => // If either are None
                for {
                  filterHeaderCount <- chainApi.getFilterHeaderCount()
                  filterCount <- chainApi.getFilterCount()
                } yield (filterHeaderCount, filterCount + 1)
            }
          newSyncing <-
            if (batchSizeFull) {
              logger.info(
                s"Received maximum amount of filters in one batch. This means we are not synced, requesting more")
              for {
                _ <- sendNextGetCompactFilterCommand(peerMsgSender,
                                                     filter.blockHash.flip)
              } yield syncing
            } else {
              val syncing = newFilterHeight < newFilterHeaderHeight
              if (!syncing) {
                logger.info(s"We are synced")
              }
              Future.successful(syncing)
            }
          // If we are not syncing or our filter batch is full, process the filters
          filterBatch = currentFilterBatch :+ filter
          (newBatch, newChainApi) <-
            if (!newSyncing || batchSizeFull) {
              val blockFilters = filterBatch.map { filter =>
                (filter.blockHash,
                 BlockFilter.fromBytes(filter.filterBytes, filter.blockHash))
              }
              logger.debug(s"Processing ${filterBatch.size} filters")
              for {
                newChainApi <- chainApi.processFilters(filterBatch)
                _ <- callbacks.executeOnCompactFiltersReceivedCallbacks(
                  logger,
                  blockFilters)
              } yield (Vector.empty, newChainApi)
            } else Future.successful((filterBatch, chainApi))

        } yield {
          this.copy(
            chainApi = newChainApi,
            currentFilterBatch = newBatch,
            syncing = newSyncing,
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
            case TypeIdentifier.MsgTx =>
              txDAO.findByHash(inv.hash).map {
                case Some(tx) =>
                  peerMsgSender.sendTransactionMessage(tx.transaction)
                case None =>
                  logger.warn(
                    s"Got request to send data with hash=${inv.hash}, but found nothing")
              }
            case other @ (TypeIdentifier.MsgBlock |
                TypeIdentifier.MsgFilteredBlock |
                TypeIdentifier.MsgCompactBlock |
                TypeIdentifier.MsgFilteredWitnessBlock |
                TypeIdentifier.MsgWitnessBlock | TypeIdentifier.MsgWitnessTx) =>
              logger.warn(
                s"Got request to send data type=$other, this is not implemented yet")

            case unassigned: MsgUnassigned =>
              logger.warn(
                s"Received unassigned message we do not understand, msg=${unassigned}")
          }

        }
        Future.successful(this)
      case HeadersMessage(count, headers) =>
        logger.info(s"Received headers message with ${count.toInt} headers")
        logger.trace(
          s"Received headers=${headers.map(_.hashBE.hex).mkString("[", ",", "]")}")
        val chainApiF = chainApi.processHeaders(headers)

        if (appConfig.nodeType == NodeType.SpvNode) {
          logger.trace(s"Requesting data for headers=${headers.length}")
          peerMsgSender.sendGetDataMessage(TypeIdentifier.MsgFilteredBlock,
                                           headers.map(_.hash): _*)
        }

        val getHeadersF = chainApiF
          .flatMap { newApi =>
            if (headers.nonEmpty) {

              val lastHeader = headers.last
              val lastHash = lastHeader.hash
              newApi.getBlockCount.map { count =>
                logger.trace(
                  s"Processed headers, most recent has height=$count and hash=$lastHash.")
              }

              if (count.toInt == HeadersMessage.MaxHeadersCount) {
                logger.info(
                  s"Received maximum amount of headers in one header message. This means we are not synced, requesting more")
                peerMsgSender
                  .sendGetHeadersMessage(lastHash)
                  .map(_ => syncing)
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
                if (
                  appConfig.nodeType == NodeType.NeutrinoNode && (!syncing || filterHeaderHeightOpt.isEmpty || filterHeightOpt.isEmpty)
                )
                  sendFirstGetCompactFilterHeadersCommand(peerMsgSender)
                else
                  Future.successful(syncing)
              }
            } else
              Future.successful(syncing)
          }

        getHeadersF.failed.map { err =>
          logger.error(s"Error when processing headers message", err)
        }

        for {
          newApi <- chainApiF
          newSyncing <- getHeadersF
          _ <- callbacks.executeOnBlockHeadersReceivedCallbacks(logger, headers)
        } yield {
          this.copy(chainApi = newApi, syncing = newSyncing)
        }
      case msg: BlockMessage =>
        val block = msg.block
        logger.info(
          s"Received block message with hash ${block.blockHeader.hash.flip.hex}")

        val newApiF = {
          chainApi
            .getHeader(block.blockHeader.hashBE)
            .flatMap { headerOpt =>
              if (headerOpt.isEmpty) {
                logger.debug("Processing block's header...")
                for {
                  processedApi <- chainApi.processHeader(block.blockHeader)
                  _ <- callbacks.executeOnBlockHeadersReceivedCallbacks(
                    logger,
                    Vector(block.blockHeader))
                } yield processedApi
              } else Future.successful(chainApi)
            }
        }

        for {
          newApi <- newApiF
          _ <-
            callbacks
              .executeOnBlockReceivedCallbacks(logger, block)
        } yield {
          this.copy(chainApi = newApi)
        }
      case TransactionMessage(tx) =>
        MerkleBuffers.putTx(tx, callbacks).flatMap { belongsToMerkle =>
          if (belongsToMerkle) {
            logger.trace(
              s"Transaction=${tx.txIdBE} belongs to merkleblock, not calling callbacks")
            Future.successful(this)
          } else {
            logger.trace(
              s"Transaction=${tx.txIdBE} does not belong to merkleblock, processing given callbacks")
            callbacks
              .executeOnTxReceivedCallbacks(logger, tx)
              .map(_ => this)
          }
        }
      case MerkleBlockMessage(merkleBlock) =>
        MerkleBuffers.putMerkle(merkleBlock)
        Future.successful(this)
      case invMsg: InventoryMessage =>
        handleInventoryMsg(invMsg = invMsg, peerMsgSender = peerMsgSender)
    }
  }

  private def sendNextGetCompactFilterHeadersCommand(
      peerMsgSender: PeerMessageSender,
      stopHash: DoubleSha256DigestBE): Future[Boolean] =
    peerMsgSender.sendNextGetCompactFilterHeadersCommand(
      chainApi = chainApi,
      filterHeaderBatchSize = chainConfig.filterHeaderBatchSize,
      stopHash = stopHash)

  private def sendFirstGetCompactFilterHeadersCommand(
      peerMsgSender: PeerMessageSender): Future[Boolean] =
    for {
      filterHeaderCount <- chainApi.getFilterHeaderCount()
      highestFilterHeaderOpt <-
        chainApi
          .getFilterHeadersAtHeight(filterHeaderCount)
          .map(_.headOption)
      highestFilterBlockHash =
        highestFilterHeaderOpt
          .map(_.blockHashBE)
          .getOrElse(DoubleSha256DigestBE.empty)
      res <- sendNextGetCompactFilterHeadersCommand(peerMsgSender,
                                                    highestFilterBlockHash)
    } yield res

  private def sendNextGetCompactFilterCommand(
      peerMsgSender: PeerMessageSender,
      stopHash: DoubleSha256DigestBE): Future[Boolean] =
    peerMsgSender.sendNextGetCompactFilterCommand(chainApi = chainApi,
                                                  filterBatchSize =
                                                    chainConfig.filterBatchSize,
                                                  stopHash = stopHash)

  private def sendFirstGetCompactFilterCommand(
      peerMsgSender: PeerMessageSender): Future[Boolean] =
    for {
      filterCount <- chainApi.getFilterCount()
      highestFilterOpt <-
        chainApi
          .getFiltersAtHeight(filterCount)
          .map(_.headOption)
      highestFilterBlockHash =
        highestFilterOpt
          .map(_.blockHashBE)
          .getOrElse(DoubleSha256DigestBE.empty)
      res <-
        sendNextGetCompactFilterCommand(peerMsgSender, highestFilterBlockHash)
    } yield res

  private def handleInventoryMsg(
      invMsg: InventoryMessage,
      peerMsgSender: PeerMessageSender): Future[DataMessageHandler] = {
    logger.info(s"Received inv=${invMsg}")
    val getData = GetDataMessage(invMsg.inventories.map {
      case Inventory(TypeIdentifier.MsgBlock, hash) =>
        // only request the merkle block if we are spv enabled
        appConfig.nodeType match {
          case NodeType.SpvNode =>
            Inventory(TypeIdentifier.MsgFilteredBlock, hash)
          case NodeType.NeutrinoNode | NodeType.FullNode =>
            Inventory(TypeIdentifier.MsgBlock, hash)
        }
      case other: Inventory => other
    })
    peerMsgSender.sendMsg(getData).map(_ => this)
  }
}
