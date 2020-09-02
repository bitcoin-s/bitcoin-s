package org.bitcoins.chain.blockchain

import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models._
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.chain.db._
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

import scala.annotation.tailrec
import scala.concurrent._

/**
  * Chain Handler is meant to be the reference implementation
  * of [[ChainApi ChainApi]], this is the entry point in to the
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
    implicit
    val chainConfig: ChainAppConfig,
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
    Future {
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
      bestHeader
    }
  }

  /** @inheritdoc */
  override def getHeader(
      hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]] = {
    blockHeaderDAO.findByHash(hash).map { header =>
      logger.debug(s"Looking for header by hash=$hash")
      val resultStr = header
        .map(h =>
          s"height=${h.height}, hash=${h.hashBE}, chain work=${h.chainWork}")
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
        // During reorgs, we can be sent a header twice
        blockchainUpdates
          .flatMap(_.successfulHeaders)
          .distinct
          .filterNot(blockchains.flatMap(_.headers).contains)
      }

      val chains = blockchainUpdates.map(_.blockchain)

      val createdF = blockHeaderDAO.createAll(headersToBeCreated)

      val newChainHandler = this.copy(blockchains = chains)

      createdF.map { _ =>
        chains.foreach { c =>
          logger.info(
            s"Processed headers from height=${c.height - headers.length} to ${c.height}. Best hash=${c.tip.hashBE.hex}")
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
  override def nextBlockHeaderBatchRange(
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
    for {
      startHeight <- startHeightF
      blockCount <- getBlockCount
      stopHeight =
        if (startHeight - 1 + batchSize > blockCount) blockCount
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
      filterHeaderCount <- getFilterHeaderCount
      stopHeight =
        if (startHeight - 1 + batchSize > filterHeaderCount)
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
      blockHeaders <-
        blockHeaderDAO
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
      _ <-
        if (
          filterHeadersToCreate.nonEmpty && filterHeadersToCreate.head.height > 0
        ) {
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
              // If the previous filter header doesn't exist it must be for the genesis block
              require(
                firstFilter.previousFilterHeaderBE == DoubleSha256DigestBE.empty && firstFilter.height == 0,
                s"Previous filter header does not exist: $firstFilter"
              )
          }
        } else FutureUtil.unit
      _ <- filterHeaderDAO.createAll(filterHeadersToCreate)
    } yield {
      val minHeightOpt = filterHeadersToCreate.minByOption(_.height)
      val maxHeightOpt = filterHeadersToCreate.maxByOption(_.height)

      (minHeightOpt, maxHeightOpt) match {
        case (Some(minHeight), Some(maxHeight)) =>
          logger.info(
            s"Processed filters headers from height=${minHeight.height} to ${maxHeight.height}. Best hash=${maxHeight.blockHashBE.hex}")
          this
        // Should never have the case where we have (Some, None) or (None, Some) because that means the vec would be both empty and non empty
        case (_, _) =>
          logger.warn("Was unable to process any filters headers")
          this
      }
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

    for {
      filterHeaders <- filterHeadersF
      _ = logger.debug(s"processFilters: filterHeaders=$filterHeaders")
      _ = require(
        filterHeaders.size == messages.size,
        s"Filter batch size does not match filter header batch size ${messages.size} != ${filterHeaders.size}")
      compactFilterDbs <- Future {
        filterHeaders.map { filterHeader =>
          findFilterDbFromMessage(filterHeader, messagesByBlockHash)
        }
      }
      _ <- filterDAO.createAll(compactFilterDbs)
    } yield {
      val minHeightOpt = compactFilterDbs.minByOption(_.height)
      val maxHeightOpt = compactFilterDbs.maxByOption(_.height)

      (minHeightOpt, maxHeightOpt) match {
        case (Some(minHeight), Some(maxHeight)) =>
          logger.info(
            s"Processed filters from height=${minHeight.height} to ${maxHeight.height}. Best hash=${maxHeight.blockHashBE.hex}")
          this
        // Should never have the case where we have (Some, None) or (None, Some) because that means the vec would be both empty and non empty
        case (_, _) =>
          logger.warn("Was unable to process any filters")
          this
      }
    }
  }

  private def findFilterDbFromMessage(
      filterHeader: CompactFilterHeaderDb,
      messagesByBlockHash: Map[
        DoubleSha256DigestBE,
        CompactFilterMessage]): CompactFilterDb = {
    messagesByBlockHash.get(filterHeader.blockHashBE) match {
      case Some(message) =>
        val filterHashBE = CryptoUtil.doubleSHA256(message.filterBytes).flip
        if (filterHashBE != filterHeader.filterHashBE) {
          val errMsg =
            s"Filter hash does not match filter header hash: ${filterHashBE} != ${filterHeader.filterHashBE}\n" +
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
    filterHeaderDAO.getBestFilterHeader.map {
      case Some(filterHeader) =>
        val height = filterHeader.height
        logger.debug(s"getFilterHeaderCount result: count=$height")
        height
      case None =>
        0
    }
  }

  /** @inheritdoc */
  override def getFilterHeadersAtHeight(
      height: Int): Future[Vector[CompactFilterHeaderDb]] =
    filterHeaderDAO.getAtHeight(height)

  /** @inheritdoc */
  override def getBestFilterHeader(): Future[Option[CompactFilterHeaderDb]] = {
    val bestFilterHeadersInChains: Vector[
      Future[Option[CompactFilterHeaderDb]]] = {
      blockchains.map { blockchain =>
        filterHeaderDAO.getBestFilterHeaderForHeaders(blockchain.toVector)
      }
    }

    val filterHeadersOptF: Future[Vector[Option[CompactFilterHeaderDb]]] = {
      Future.sequence(bestFilterHeadersInChains)
    }

    for {
      filterHeaders <- filterHeadersOptF
      flattened = filterHeaders.flatten
      result <-
        if (flattened.isEmpty) {
          bestFilterHeaderSearch()
        } else {
          Future.successful(flattened.maxByOption(_.height))
        }
    } yield {
      result
    }
  }

  /**
    * This method retrieves the best [[CompactFilterHeaderDb]] from the database
    * without any blockchain context, and then uses the [[CompactFilterHeaderDb.blockHashBE]]
    * to query our block headers database looking for a filter header that is in the best chain
    * @return
    */
  private def bestFilterHeaderSearch(): Future[
    Option[CompactFilterHeaderDb]] = {
    val bestFilterHeaderOptF = filterHeaderDAO.getBestFilterHeader

    //get best blockchain around our latest filter header
    val blockchainOptF: Future[Option[Blockchain]] = {
      for {
        bestFilterHeaderOpt <- bestFilterHeaderOptF
        blockchains <- {
          bestFilterHeaderOpt match {
            case Some(bestFilterHeader) =>
              //get blockchains from our current best filter header to
              //the next POW of interval, this should be enough to determine
              //what is the best chain!
              blockHeaderDAO.getBlockchainsBetweenHeights(
                from =
                  bestFilterHeader.height - chainConfig.chain.difficultyChangeInterval,
                to =
                  bestFilterHeader.height + chainConfig.chain.difficultyChangeInterval)
            case None =>
              Future.successful(Vector.empty)
          }
        }
      } yield {
        if (blockchains.isEmpty) {
          None
        } else {
          blockchains.maxByOption(_.tip.chainWork)
        }
      }
    }

    val filterHeadersOptF: Future[Option[CompactFilterHeaderDb]] = {
      for {
        blockchainOpt <- blockchainOptF
        bestHeadersForChainFOpt = {
          blockchainOpt.map(b =>
            filterHeaderDAO.getBestFilterHeaderForHeaders(b.toVector))
        }
        bestHeadersForChain <- bestHeadersForChainFOpt match {
          case Some(f) => f
          case None    => Future.successful(None)
        }
      } yield bestHeadersForChain
    }

    filterHeadersOptF
  }

  /** @inheritdoc */
  override def getFilterHeader(
      blockHash: DoubleSha256DigestBE): Future[Option[CompactFilterHeaderDb]] =
    filterHeaderDAO.findByBlockHash(blockHash)

  /** @inheritdoc */
  override def getFilterCount: Future[Int] = {
    logger.debug(s"Querying for filter count")
    filterDAO.getBestFilter.map {
      case Some(filter) =>
        val height = filter.height
        logger.debug(s"getFilterCount result: count=$height")
        height
      case None => 0
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
          tips <- blockHeaderDAO.chainTips
          getNAncestorsFs = tips.map { tip =>
            blockHeaderDAO.getNAncestors(tip.hashBE, tip.height - blockHeight)
          }
          ancestorChains <- Future.sequence(getNAncestorsFs)
        } yield {
          val confs = ancestorChains.flatMap { chain =>
            if (chain.last.hashBE == blockHash) {
              Some(chain.head.height - blockHeight + 1)
            } else None
          }

          if (confs.nonEmpty) {
            Some(confs.max)
          } else None
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
      first100MissingWork =
        first100.nonEmpty && first100.exists(_.chainWork == BigInt(0))
      isMissingWork <- {
        if (first100MissingWork) {
          Future.successful(true)
        } else {
          for {
            height <- blockHeaderDAO.maxHeight
            last100 <- blockHeaderDAO.getBetweenHeights(height - 100, height)
            last100MissingWork =
              last100.nonEmpty && last100.exists(_.chainWork == BigInt(0))
          } yield last100MissingWork
        }
      }

    } yield isMissingWork
  }

  @tailrec
  private def calcChainWork(
      remainingHeaders: Vector[BlockHeaderDb],
      accum: Vector[BlockHeaderDb],
      lastHeaderWithWorkInDb: BlockHeaderDb): Vector[BlockHeaderDb] = {
    if (remainingHeaders.isEmpty) {
      accum
    } else {
      val header = remainingHeaders.head

      val currentChainWork = {
        accum.lastOption.map(_.chainWork) match {
          case Some(prevWork) =>
            prevWork
          case None =>
            // this should be the case where the accum is
            //empty, so this header is the last one we have
            //stored in the database
            lastHeaderWithWorkInDb.chainWork
        }
      }
      val newChainWork =
        currentChainWork + Pow.getBlockProof(header.blockHeader)
      val newHeader = header.copy(chainWork = newChainWork)
      calcChainWork(remainingHeaders.tail,
                    accum :+ newHeader,
                    lastHeaderWithWorkInDb)
    }
  }

  private def getBatchForRecalc(
      startHeight: Int,
      maxHeight: Int,
      batchSize: Int): Future[Vector[Blockchain]] = {
    val batchEndHeight = Math.min(maxHeight, startHeight + batchSize - 1)
    val headersToCalcF = {
      logger.trace(s"Fetching from=$startHeight to=$batchEndHeight")
      blockHeaderDAO.getBlockchainsBetweenHeights(from = startHeight,
                                                  to = batchEndHeight)
    }

    headersToCalcF
  }

  /** Creates [[numBatches]] of requests to the database fetching [[batchSize]] headers
    * starting at [[batchStartHeight]]. These are executed in parallel. After all are fetched
    * we join them into one future and return it.
    */
  private def batchAndGetBlockchains(
      batchSize: Int,
      batchStartHeight: Int,
      maxHeight: Int,
      numBatches: Int): Future[Vector[Blockchain]] = {
    var counter = batchStartHeight
    val range = 0.until(numBatches)
    val batchesNested: Vector[Future[Vector[Blockchain]]] = range.map { _ =>
      val f =
        if (counter <= maxHeight) {
          getBatchForRecalc(startHeight = counter,
                            maxHeight = maxHeight,
                            batchSize = batchSize)
        } else {
          Future.successful(Vector.empty)
        }
      counter += batchSize
      f
    }.toVector

    Future
      .sequence(batchesNested)
      .map(_.flatten)
  }

  private def runRecalculateChainWork(
      maxHeight: Int,
      lastHeader: BlockHeaderDb): Future[Vector[BlockHeaderDb]] = {
    val currentHeight = lastHeader.height
    val numBatches = 1
    val batchSize =
      chainConfig.appConfig.chain.difficultyChangeInterval / numBatches
    if (currentHeight >= maxHeight) {
      Future.successful(Vector.empty)
    } else {
      val batchStartHeight = currentHeight + 1

      val headersToCalcF = batchAndGetBlockchains(
        batchSize = batchSize,
        batchStartHeight = batchStartHeight,
        maxHeight = maxHeight,
        numBatches = numBatches
      )

      for {
        headersToCalc <- headersToCalcF
        _ = headersToCalc.headOption.map { h =>
          logger.info(
            s"Recalculating chain work... current height: ${h.height} maxHeight=$maxHeight")
        }
        headersWithWork = {
          headersToCalc.flatMap { chain =>
            calcChainWork(remainingHeaders = chain.headers.sortBy(_.height),
                          accum = Vector.empty,
                          lastHeaderWithWorkInDb = lastHeader)
          }
        }

        //unfortunately on sqlite there is a bottle neck here
        //sqlite allows you to read in parallel but only write
        //sequentially https://stackoverflow.com/a/23350768/967713
        //so while it looks like we are executing in parallel
        //in reality there is only one thread that can write to the db
        //at a single time
        _ = logger.trace(
          s"Upserting from height=${headersWithWork.headOption.map(_.height)} " +
            s"to height=${headersWithWork.lastOption.map(_.height)}")
        _ <- FutureUtil.batchExecute(
          headersWithWork,
          blockHeaderDAO.upsertAll,
          Vector.empty,
          batchSize
        )
        _ = logger.trace(
          s"Done upserting from height=${headersWithWork.headOption.map(
            _.height)} to height=${headersWithWork.lastOption.map(_.height)}")
        next <- runRecalculateChainWork(maxHeight, headersWithWork.last)
      } yield {
        next
      }
    }
  }

  def recalculateChainWork: Future[ChainHandler] = {
    logger.info("Calculating chain work for previous blocks")

    val maxHeightF = blockHeaderDAO.maxHeight
    val startHeightF = blockHeaderDAO.getLowestNoWorkHeight
    val startF = for {
      startHeight <- startHeightF
      headers <- {
        if (startHeight == 0) {
          val genesisHeaderF = blockHeaderDAO.getAtHeight(0)
          genesisHeaderF.flatMap { h =>
            require(h.length == 1, s"Should only have one genesis header!")
            calculateChainWorkGenesisBlock(h.head)
              .map(Vector(_))
          }
        } else {
          blockHeaderDAO.getAtHeight(startHeight - 1)
        }
      }
    } yield headers

    val resultF = for {
      maxHeight <- maxHeightF
      start <- startF
      _ <- runRecalculateChainWork(maxHeight, start.head)
      newBlockchains <- blockHeaderDAO.getBlockchains()
    } yield {
      logger.info("Finished calculating chain work")
      this.copy(blockchains = newBlockchains)
    }

    resultF.failed.foreach { err =>
      logger.error(s"Failed to recalculate chain work", err)
    }

    resultF
  }

  /** Calculates the chain work for the genesis header */
  private def calculateChainWorkGenesisBlock(
      genesisHeader: BlockHeaderDb): Future[BlockHeaderDb] = {
    val expectedWork = Pow.getBlockProof(genesisHeader.blockHeader)
    val genesisWithWork = genesisHeader.copy(chainWork = expectedWork)
    blockHeaderDAO.update(genesisWithWork)
  }
}

object ChainHandler {

  /** Constructs a [[ChainHandler chain handler]] from the state in the database
    * This gives us the guaranteed latest state we have in the database
    */
  def fromDatabase(
      blockHeaderDAO: BlockHeaderDAO,
      filterHeaderDAO: CompactFilterHeaderDAO,
      filterDAO: CompactFilterDAO)(implicit
      ec: ExecutionContext,
      chainConfig: ChainAppConfig): Future[ChainHandler] = {
    val bestChainsF = blockHeaderDAO.getBlockchains()

    bestChainsF.map(chains =>
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
      blockchains: Blockchain)(implicit
      ec: ExecutionContext,
      chainConfig: ChainAppConfig): ChainHandler = {
    new ChainHandler(blockHeaderDAO = blockHeaderDAO,
                     filterHeaderDAO = filterHeaderDAO,
                     filterDAO = filterDAO,
                     blockchains = Vector(blockchains),
                     blockFilterCheckpoints = Map.empty)
  }
}
