package org.bitcoins.chain.blockchain

import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models._
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.api.ChainQueryApi.FilterResponse
import org.bitcoins.core.gcs.FilterHeader
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.{
  CryptoUtil,
  DoubleSha256Digest,
  DoubleSha256DigestBE
}

import scala.concurrent._

/**
  * Chain Handler is meant to be the reference implementation
  * of [[org.bitcoins.chain.api.ChainApi ChainApi]], this is the entry point in to the
  * chain project.
  *
  * @param blockHeaderDAO block header DB
  * @param filterHeaderDAO filter header DB
  * @param filterDAO filter DB
  * @param blockchains current blockchains
  * @param blockFilterCheckpoints compact filter checkpoints for filter header verification in form of a map (block header hash -> filter header hash)
  * @param chainConfig config file
  */
case class ChainHandler(
    blockHeaderDAO: BlockHeaderDAO,
    filterHeaderDAO: CompactFilterHeaderDAO,
    filterDAO: CompactFilterDAO,
    blockchains: Vector[Blockchain],
    blockFilterCheckpoints: Map[DoubleSha256DigestBE, DoubleSha256DigestBE])(
    implicit val chainConfig: ChainAppConfig,
    executionContext: ExecutionContext)
    extends ChainApi
    with ChainVerificationLogger {

  /** @inheritdoc */
  override def getBlockCount(): Future[Int] = {
    logger.debug(s"Querying for block count")
    blockHeaderDAO.bestHeight.map { height =>
      logger.debug(s"getBlockCount result: count=$height")
      height
    }
  }

  override def getBestBlockHeader(): Future[BlockHeaderDb] = {
    logger.debug(s"Querying for best block hash")
    //https://bitcoin.org/en/glossary/block-chain
    val groupedChains = blockchains.groupBy(_.tip.chainWork)
    val maxWork = groupedChains.keys.max
    val chains = groupedChains(maxWork)

    val bestHeader: BlockHeaderDb = chains match {
      case Vector() =>
        // This should never happen
        val errMsg = s"Did not find blockchain with work $maxWork"
        logger.error(errMsg)
        throw new RuntimeException(errMsg)
      case chain +: Vector() =>
        chain.tip
      case chain +: rest =>
        logger.warn(
          s"We have multiple competing blockchains: ${(chain +: rest).map(_.tip.hashBE.hex).mkString(", ")}")
        chain.tip
    }
    Future.successful(bestHeader)
  }

  /** @inheritdoc */
  override def getHeader(
      hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]] = {
    blockHeaderDAO.findByHash(hash).map { header =>
      logger.debug(s"Looking for header by hash=$hash")
      val resultStr = header
        .map(h => s"height=${h.height}, hash=${h.hashBE}")
        .getOrElse("None")
      logger.debug(s"getHeader result: $resultStr")
      header
    }
  }

  /** @inheritdoc */
  override def processHeaders(
      headers: Vector[BlockHeader]): Future[ChainApi] = {
    if (headers.isEmpty) {
      Future.successful(this)
    } else {
      val blockchainUpdates: Vector[BlockchainUpdate] = {
        Blockchain.connectHeadersToChains(headers, blockchains)
      }

      val headersToBeCreated = {
        blockchainUpdates.flatMap(_.successfulHeaders).distinct
      }

      val chains = blockchainUpdates.map(_.blockchain)

      val createdF = blockHeaderDAO.createAll(headersToBeCreated)

      val newChainHandler = this.copy(blockchains = chains)

      createdF.map { _ =>
        chains.foreach { c =>
          logger.info(
            s"Processed headers from height=${c(headers.length - 1).height} to ${c.height}. Best hash=${c.tip.hashBE.hex}")
        }
        newChainHandler
      }
    }
  }

  /**
    * @inheritdoc
    */
  override def getBestBlockHash(): Future[DoubleSha256DigestBE] = {
    getBestBlockHeader().map(_.hashBE)
  }

  /** @inheritdoc */
  override def nextHeaderBatchRange(
      prevStopHash: DoubleSha256DigestBE,
      batchSize: Int): Future[Option[(Int, DoubleSha256Digest)]] = {
    val startHeightF = if (prevStopHash == DoubleSha256DigestBE.empty) {
      Future.successful(0)
    } else {
      for {
        prevStopHeaderOpt <- getHeader(prevStopHash)
        prevStopHeader = prevStopHeaderOpt.getOrElse(
          throw UnknownBlockHash(s"Unknown block hash ${prevStopHash}"))
      } yield prevStopHeader.height + 1
    }
    val blockCountF = getBlockCount()
    for {
      startHeight <- startHeightF
      blockCount <- blockCountF
      stopHeight = if (startHeight - 1 + batchSize > blockCount) blockCount
      else startHeight - 1 + batchSize
      stopBlockOpt <- getHeadersAtHeight(stopHeight).map(_.headOption)
      stopBlock = stopBlockOpt.getOrElse(
        throw UnknownBlockHeight(s"Unknown header height ${stopHeight}"))
    } yield {
      if (startHeight > stopHeight)
        None
      else
        Some((startHeight, stopBlock.hashBE.flip))
    }
  }

  /** @inheritdoc */
  override def nextFilterHeaderBatchRange(
      prevStopHash: DoubleSha256DigestBE,
      batchSize: Int): Future[Option[(Int, DoubleSha256Digest)]] = {
    val filterHeaderCountF = getFilterHeaderCount
    val startHeightF = if (prevStopHash == DoubleSha256DigestBE.empty) {
      Future.successful(0)
    } else {
      for {
        prevStopHeaderOpt <- getFilterHeader(prevStopHash)
        prevStopHeader = prevStopHeaderOpt.getOrElse(
          throw UnknownBlockHash(s"Unknown block hash ${prevStopHash}"))
      } yield prevStopHeader.height + 1
    }

    for {
      startHeight <- startHeightF
      filterHeaderCount <- filterHeaderCountF
      stopHeight = if (startHeight - 1 + batchSize > filterHeaderCount)
        filterHeaderCount
      else startHeight - 1 + batchSize
      stopBlockOpt <- getFilterHeadersAtHeight(stopHeight).map(_.headOption)
      stopBlock = stopBlockOpt.getOrElse(
        throw UnknownBlockHeight(s"Unknown filter header height ${stopHeight}"))
    } yield {
      if (startHeight > stopHeight)
        None
      else
        Some((startHeight, stopBlock.blockHashBE.flip))
    }
  }

  /** @inheritdoc */
  override def processFilterHeaders(
      filterHeaders: Vector[FilterHeader],
      stopHash: DoubleSha256DigestBE): Future[ChainApi] = {

    val filterHeadersToCreateF: Future[Vector[CompactFilterHeaderDb]] = for {
      blockHeaders <- blockHeaderDAO
        .getNAncestors(childHash = stopHash, n = filterHeaders.size - 1)
        .map(_.sortBy(_.height))
    } yield {
      if (blockHeaders.size != filterHeaders.size) {
        throw UnknownBlockHash(
          s"Filter header batch size does not match block header batch size ${filterHeaders.size} != ${blockHeaders.size}")
      }
      blockHeaders.indices.toVector.map { i =>
        val blockHeader = blockHeaders(i)
        val filterHeader = filterHeaders(i)
        CompactFilterHeaderDbHelper.fromFilterHeader(filterHeader,
                                                     blockHeader.hashBE,
                                                     blockHeader.height)
      }
    }

    for {
      filterHeadersToCreate <- filterHeadersToCreateF
      _ <- if (filterHeadersToCreate.nonEmpty && filterHeadersToCreate.head.height > 0) {
        val firstFilter = filterHeadersToCreate.head
        val filterHashFOpt = filterHeaderDAO
          .findByHash(firstFilter.previousFilterHeaderBE)
        filterHashFOpt.map {
          case Some(prevHeader) =>
            require(
              prevHeader.height == firstFilter.height - 1,
              s"Unexpected previous header's height: ${prevHeader.height} != ${filterHeadersToCreate.head.height - 1}"
            )
          case None =>
            if (firstFilter.previousFilterHeaderBE == DoubleSha256DigestBE.empty && firstFilter.height == 0) {
              //we are ok, according to BIP157 the previous the genesis filter's prev hash should
              //be the empty hash
              ()
            } else {
              sys.error(s"Previous filter header does not exist: $firstFilter")
            }
        }
      } else FutureUtil.unit
      _ <- filterHeaderDAO.createAll(filterHeadersToCreate)
    } yield {
      val minHeight = filterHeadersToCreate.minBy(_.height)
      val maxHeight = filterHeadersToCreate.maxBy(_.height)
      logger.info(
        s"Processed filters headers from height=${minHeight.height} to ${maxHeight.height}. Best hash=${maxHeight.blockHashBE}")
      this
    }
  }

  /** @inheritdoc */
  override def processFilters(
      messages: Vector[CompactFilterMessage]): Future[ChainApi] = {

    logger.debug(s"processFilters: messages=${messages}")
    val filterHeadersF = filterHeaderDAO
      .findAllByBlockHashes(messages.map(_.blockHash.flip))
      .map(_.sortBy(_.height))

    val messagesByBlockHash: Map[DoubleSha256DigestBE, CompactFilterMessage] =
      messages.groupBy(_.blockHash.flip).map {
        case (blockHash, messages) =>
          if (messages.size > 1)
            throw DuplicateFilters("Attempt to process duplicate filters")
          (blockHash, messages.head)
      }

    val sizeCheckF = for {
      filterHeaders <- filterHeadersF
      _ = logger.debug(s"processFilters: filterHeaders=${filterHeaders}")

      _ <- if (filterHeaders.size != messages.size) {
        Future.failed(new UnknownBlockHash(
          s"Filter batch size does not match filter header batch size ${messages.size} != ${filterHeaders.size}"))
      } else {
        FutureUtil.unit
      }
    } yield ()

    for {
      filterHeaders <- filterHeadersF
      _ <- sizeCheckF
      compactFilterDbs <- Future {
        filterHeaders.map { filterHeader =>
          findFilterDbFromMessage(filterHeader, messagesByBlockHash)
        }
      }
      _ <- filterDAO.createAll(compactFilterDbs)
    } yield {
      val minHeight = compactFilterDbs.minBy(_.height)
      val maxHeight = compactFilterDbs.maxBy(_.height)
      logger.info(
        s"Processed filters from height=${minHeight.height} to ${maxHeight.height}. Best hash=${maxHeight.blockHashBE}")
      this
    }
  }

  private def findFilterDbFromMessage(
      filterHeader: CompactFilterHeaderDb,
      messagesByBlockHash: Map[DoubleSha256DigestBE, CompactFilterMessage]): CompactFilterDb = {
    messagesByBlockHash.get(filterHeader.blockHashBE) match {
      case Some(message) =>
        val filterHashBE = CryptoUtil.doubleSHA256(message.filterBytes).flip
        if (filterHashBE != filterHeader.filterHashBE) {
          val errMsg = s"Filter hash does not match filter header hash: ${filterHashBE} != ${filterHeader.filterHashBE}\n" +
            s"filter=${message.filterBytes.toHex}\nblock hash=${message.blockHash}\nfilterHeader=${filterHeader}"
          logger.warn(errMsg)
          throw UnknownFilterHash(errMsg)
        }
        val filter =
          CompactFilterDbHelper.fromFilterBytes(message.filterBytes,
                                                filterHeader.blockHashBE,
                                                filterHeader.height)
        filter
      case None =>
        throw UnknownBlockHash(
          s"Unknown block hash ${filterHeader.blockHashBE}")
    }
  }

  /** @inheritdoc */
  override def processCheckpoints(
      checkpoints: Vector[DoubleSha256DigestBE],
      blockHash: DoubleSha256DigestBE): Future[ChainApi] = {

    val blockHeadersF: Future[Seq[BlockHeaderDb]] = Future
      .traverse(checkpoints.indices.toVector) { i =>
        blockHeaderDAO.getAtHeight(i * 1000)
      }
      .map(headers => headers.map(_.head))

    for {
      blockHeaders <- blockHeadersF
    } yield {
      val checkpointsWithBlocks = checkpoints.zip(blockHeaders)

      val updatedCheckpoints =
        checkpointsWithBlocks.foldLeft(blockFilterCheckpoints) { (res, pair) =>
          val (filterHeaderHash, blockHeader) = pair
          res.updated(blockHeader.hashBE, filterHeaderHash)
        }

      this.copy(blockFilterCheckpoints = updatedCheckpoints)
    }

  }

  /** @inheritdoc */
  override def getFilter(
      blockHash: DoubleSha256DigestBE): Future[Option[CompactFilterDb]] = {
    filterDAO.findByBlockHash(blockHash)
  }

  /** @inheritdoc */
  override def getHeadersAtHeight(height: Int): Future[Vector[BlockHeaderDb]] =
    blockHeaderDAO.getAtHeight(height)

  /** @inheritdoc */
  override def getFilterHeaderCount: Future[Int] = {
    logger.debug(s"Querying for filter header count")
    filterHeaderDAO.getBestFilter.map { filterHeader =>
      val height = filterHeader.height
      logger.debug(s"getFilterCount result: count=$height")
      height
    }
  }

  /** @inheritdoc */
  override def getFilterHeadersAtHeight(
      height: Int): Future[Vector[CompactFilterHeaderDb]] =
    filterHeaderDAO.getAtHeight(height)

  /** @inheritdoc */
  override def getBestFilterHeader(): Future[CompactFilterHeaderDb] = {
    filterHeaderDAO.getBestFilter
  }

  /** @inheritdoc */
  override def getFilterHeader(
      blockHash: DoubleSha256DigestBE): Future[Option[CompactFilterHeaderDb]] =
    filterHeaderDAO.findByBlockHash(blockHash)

  /** @inheritdoc */
  override def getFilterCount: Future[Int] = {
    logger.debug(s"Querying for filter count")
    filterDAO.getBestFilter.map { filter =>
      val height = filter.height
      logger.debug(s"getFilterCount result: count=$height")
      height
    }
  }

  /** @inheritdoc */
  override def getFiltersAtHeight(
      height: Int): Future[Vector[CompactFilterDb]] =
    filterDAO.getAtHeight(height)

  /** @inheritdoc */
  override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
    blockStamp match {
      case blockHeight: BlockStamp.BlockHeight =>
        Future.successful(blockHeight.height)
      case blockHash: BlockStamp.BlockHash =>
        getHeader(blockHash.hash).map { header =>
          header
            .map(_.height)
            .getOrElse(
              throw UnknownBlockHash(s"Unknown block hash ${blockHash.hash}"))
        }
      case blockTime: BlockStamp.BlockTime =>
        Future.failed(new RuntimeException(s"Not implemented: $blockTime"))
    }

  override def epochSecondToBlockHeight(time: Long): Future[Int] =
    blockHeaderDAO.findClosestToTime(time = UInt32(time)).map(_.height)

  /** @inheritdoc */
  override def getBlockHeight(
      blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
    getHeader(blockHash).map(_.map(_.height))

  /** @inheritdoc */
  override def getNumberOfConfirmations(
      blockHash: DoubleSha256DigestBE): Future[Option[Int]] = {
    getBlockHeight(blockHash).flatMap {
      case None => FutureUtil.none
      case Some(blockHeight) =>
        for {
          tipHash <- getBestBlockHash()
          tipHeightOpt <- getBlockHeight(tipHash)
        } yield {
          tipHeightOpt.map(tipHeight => tipHeight - blockHeight + 1)
        }
    }
  }

  override def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[FilterResponse]] =
    filterDAO
      .getBetweenHeights(startHeight, endHeight)
      .map(dbos =>
        dbos.map(dbo =>
          FilterResponse(dbo.golombFilter, dbo.blockHashBE, dbo.height)))

  /** @inheritdoc */
  override def getHeadersBetween(
      from: BlockHeaderDb,
      to: BlockHeaderDb): Future[Vector[BlockHeaderDb]] = {
    logger.info(s"Finding headers from=$from to=$to")
    def loop(
        currentF: Future[BlockHeaderDb],
        accum: Vector[BlockHeaderDb]): Future[Vector[BlockHeaderDb]] = {
      currentF.flatMap { current =>
        if (current.previousBlockHashBE == from.hashBE) {
          Future.successful(current +: accum)
        } else {
          val nextOptF = getHeader(current.previousBlockHashBE)
          val nextF = nextOptF.map(_.getOrElse(
            sys.error(s"Could not find header=${current.previousBlockHashBE}")))
          loop(nextF, current +: accum)
        }
      }
    }
    loop(Future.successful(to), Vector.empty)
  }

  def isMissingChainWork: Future[Boolean] = {
    for {
      first100 <- blockHeaderDAO.getBetweenHeights(0, 100)
      first100MissingWork = first100.nonEmpty && first100.exists(
        _.chainWork == BigInt(0))
      isMissingWork <- {
        if (first100MissingWork) {
          Future.successful(true)
        } else {
          for {
            height <- blockHeaderDAO.maxHeight
            last100 <- blockHeaderDAO.getBetweenHeights(height - 100, height)
            last100MissingWork = last100.nonEmpty && last100.exists(
              _.chainWork == BigInt(0))
          } yield last100MissingWork
        }
      }

    } yield isMissingWork
  }

  def recalculateChainWork: Future[ChainHandler] = {
    logger.info("Calculating chain work for previous blocks")

    val batchSize = chainConfig.chain.difficultyChangeInterval

    def loop(
        remainingHeaders: Vector[BlockHeaderDb],
        accum: Vector[BlockHeaderDb]): Future[Vector[BlockHeaderDb]] = {
      if (remainingHeaders.isEmpty) {
        blockHeaderDAO.upsertAll(accum.takeRight(batchSize))
      } else {
        val header = remainingHeaders.head

        val currentChainWork =
          accum.lastOption.map(_.chainWork).getOrElse(BigInt(0))
        val newChainWork = currentChainWork + Pow.getBlockProof(
          header.blockHeader)
        val newHeader = header.copy(chainWork = newChainWork)

        // Add the last batch to the database and create log
        if (header.height % batchSize == 0) {
          logger.info(
            s"Recalculating chain work... current height: ${header.height}")
          val updated = accum :+ newHeader
          // updated the latest batch
          blockHeaderDAO
            .upsertAll(updated.takeRight(batchSize))
            .flatMap(
              _ =>
                loop(
                  remainingHeaders.tail,
                  updated.takeRight(batchSize)
                ))
        } else {
          loop(remainingHeaders.tail, accum :+ newHeader)
        }
      }
    }

    def loop2(
        maxHeight: Int,
        accum: Vector[BlockHeaderDb]): Future[Vector[BlockHeaderDb]] = {

      val highestHeaderOpt = accum.maxByOption(_.height)
      val currentHeight = highestHeaderOpt.map(_.height).getOrElse(0)

      if (currentHeight >= maxHeight) {
        Future.successful(accum)
      } else {
        val (batchStartHeight, prev) = if (currentHeight == 0) {
          (0, Vector.empty)
        } else {
          (currentHeight + 1, Vector(highestHeaderOpt).flatten)
        }

        val batchEndHeight = Math.min(maxHeight, currentHeight + batchSize)
        for {
          headersToCalc <- blockHeaderDAO.getBetweenHeights(batchStartHeight,
                                                            batchEndHeight)
          sortedHeaders = headersToCalc.sortBy(_.height)
          headersWithWork <- loop(sortedHeaders, prev)
          next <- loop2(maxHeight, headersWithWork)
        } yield next
      }
    }

    for {
      maxHeight <- blockHeaderDAO.maxHeight
      startHeight <- blockHeaderDAO.getLowestNoWorkHeight
      start <- if (startHeight == 0) {
        Future.successful(Vector.empty)
      } else {
        blockHeaderDAO.getAtHeight(startHeight - 1)
      }
      _ <- loop2(maxHeight, start)
      newBlockchains <- blockHeaderDAO.getBlockchains()
    } yield {
      logger.info("Finished calculating chain work")
      this.copy(blockchains = newBlockchains)
    }
  }
}

object ChainHandler {

  /** Constructs a [[ChainHandler chain handler]] from the state in the database
    * This gives us the guaranteed latest state we have in the database
    * */
  def fromDatabase(
      blockHeaderDAO: BlockHeaderDAO,
      filterHeaderDAO: CompactFilterHeaderDAO,
      filterDAO: CompactFilterDAO)(
      implicit ec: ExecutionContext,
      chainConfig: ChainAppConfig): Future[ChainHandler] = {
    val bestChainsF = blockHeaderDAO.getBlockchains()

    bestChainsF.map(
      chains =>
        new ChainHandler(blockHeaderDAO = blockHeaderDAO,
                         filterHeaderDAO = filterHeaderDAO,
                         filterDAO = filterDAO,
                         blockchains = chains,
                         blockFilterCheckpoints = Map.empty))
  }

  def apply(
      blockHeaderDAO: BlockHeaderDAO,
      filterHeaderDAO: CompactFilterHeaderDAO,
      filterDAO: CompactFilterDAO,
      blockchains: Blockchain)(
      implicit ec: ExecutionContext,
      chainConfig: ChainAppConfig): ChainHandler = {
    new ChainHandler(blockHeaderDAO = blockHeaderDAO,
                     filterHeaderDAO = filterHeaderDAO,
                     filterDAO = filterDAO,
                     blockchains = Vector(blockchains),
                     blockFilterCheckpoints = Map.empty)
  }
}
