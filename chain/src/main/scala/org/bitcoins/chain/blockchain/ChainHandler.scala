package org.bitcoins.chain.blockchain

import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models._
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
    blockHeaderDAO.maxHeight.map { height =>
      logger.debug(s"getBlockCount result: count=$height")
      height
    }
  }

  override def getBestBlockHeader(): Future[BlockHeaderDb] = {
    for {
      hash <- getBestBlockHash()
      headerOpt <- getHeader(hash)
    } yield headerOpt match {
      case None =>
        throw new RuntimeException(
          s"We found best hash=${hash.hex} but could not retrieve the full header!!!")
      case Some(header) => header
    }
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
    logger.debug(s"Querying for best block hash")
    //naive implementation, this is looking for the tip with the _most_ proof of work
    //this does _not_ mean that it is on the chain that has the most work
    //TODO: Enhance this in the future to return the "heaviest" header
    //https://bitcoin.org/en/glossary/block-chain
    val groupedChains = blockchains.groupBy(_.tip.height)
    val maxHeight = groupedChains.keys.max
    val chains = groupedChains(maxHeight)

    val hashBE: DoubleSha256DigestBE = chains match {
      case Vector() =>
        val errMsg = s"Did not find blockchain with height $maxHeight"
        logger.error(errMsg)
        throw new RuntimeException(errMsg)
      case chain +: Vector() =>
        chain.tip.hashBE
      case chain +: rest =>
        logger.warn(
          s"We have multiple competing blockchains: ${(chain +: rest).map(_.tip.hashBE.hex).mkString(", ")}")
        chain.tip.hashBE
    }
    Future.successful(hashBE)
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
    val blockCountF = getBlockCount
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
    filterHeaderDAO.maxHeight.map { height =>
      logger.debug(s"getFilterHeaderCount result: count=$height")
      height
    }
  }

  /** @inheritdoc */
  override def getFilterHeadersAtHeight(
      height: Int): Future[Vector[CompactFilterHeaderDb]] =
    filterHeaderDAO.getAtHeight(height)

  /** @inheritdoc */
  override def getBestFilterHeader(): Future[CompactFilterHeaderDb] = {
    //this seems realy brittle, is there a guarantee
    //that the highest filter header count is our best filter header?
    val filterCountF = getFilterHeaderCount()
    val ourBestFilterHeader = for {
      count <- filterCountF
      filterHeader <- getFilterHeadersAtHeight(count)
    } yield {
      //TODO: Figure out what the best way to select
      //the best filter header is if we have competing
      //chains. (Same thing applies to getBestBlockHash()
      //for now, just do the dumb thing and pick the first one
      filterHeader match {
        case tip1 +: _ +: _ =>
          tip1
        case tip +: _ =>
          tip
        case Vector() =>
          sys.error(s"No filter headers found in database!")
      }
    }

    ourBestFilterHeader
  }

  /** @inheritdoc */
  override def getFilterHeader(
      blockHash: DoubleSha256DigestBE): Future[Option[CompactFilterHeaderDb]] =
    filterHeaderDAO.findByBlockHash(blockHash)

  /** @inheritdoc */
  override def getFilterCount: Future[Int] = {
    logger.debug(s"Querying for filter count")
    filterDAO.maxHeight.map { height =>
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
