package org.bitcoins.chain.blockchain

import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models._
import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.gcs.{BlockFilter, FilterHeader}
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.CryptoUtil

import scala.concurrent.{ExecutionContext, Future}

/**
  * Chain Handler is meant to be the reference implementation
  * of [[org.bitcoins.chain.api.ChainApi ChainApi]], this is the entry point in to the
  * chain project.
  *
  * @param blockHeaderDAO block header DB
  * @param filterHeaderDAO filter header DB
  * @param filterDAO filter DB
  * @param blockchains current blockchains
  * @param blockFilterCheckpoints compact filter checkpoints for filter header verification
  * @param chainConfig config file
  */
case class ChainHandler(
    blockHeaderDAO: BlockHeaderDAO,
    filterHeaderDAO: CompactFilterHeaderDAO,
    filterDAO: CompactFilterDAO,
    blockchains: Vector[Blockchain],
    blockFilterCheckpoints: Map[DoubleSha256DigestBE, DoubleSha256DigestBE])(
    implicit private[chain] val chainConfig: ChainAppConfig)
    extends ChainApi
    with ChainVerificationLogger {

  override def getBlockCount(implicit ec: ExecutionContext): Future[Long] = {
    logger.debug(s"Querying for block count")
    blockHeaderDAO.maxHeight.map { height =>
      logger.debug(s"getBlockCount result: count=$height")
      height
    }
  }

  override def getHeader(hash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[Option[BlockHeaderDb]] = {
    blockHeaderDAO.findByHash(hash).map { header =>
      logger.debug(s"Looking for header by hash=$hash")
      val resultStr = header
        .map(h => s"height=${h.height}, hash=${h.hashBE}")
        .getOrElse("None")
      logger.debug(s"getHeader result: $resultStr")
      header
    }
  }

  override def getNthHeader(hash: DoubleSha256DigestBE, count: Int)(
    implicit ec: ExecutionContext): Future[Option[BlockHeaderDb]] = {
    val range = 0.until(count)
    range.foldLeft(getHeader(hash)) { (headerF, _) =>
      headerF.flatMap {
        case Some(header) => getHeader(header.previousBlockHashBE)
        case None => headerF
      }
    }
  }

  /** @inheritdoc */
  override def processHeaders(headers: Vector[BlockHeader])(
      implicit ec: ExecutionContext): Future[ChainApi] = {
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
      newChainHandler
    }
  }

  /**
    * @inheritdoc
    */
  override def getBestBlockHash(
      implicit ec: ExecutionContext): Future[DoubleSha256DigestBE] = {
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

  override def nextBatchRange(prevStopHash: DoubleSha256DigestBE, batchSize: Long)(implicit ec: ExecutionContext): Future[Option[(Int, DoubleSha256Digest)]] = {
    val startHeightF = if (prevStopHash == DoubleSha256DigestBE.empty) {
      Future.successful(0)
    } else {
      for {
        prevStopHeaderOpt <- getHeader(prevStopHash)
        prevStopHeader = prevStopHeaderOpt.getOrElse(throw new RuntimeException(s"Unknown block hash ${prevStopHash}"))
      } yield prevStopHeader.height + 1
    }
    for {
      startHeight <- startHeightF
      blockCount <- getBlockCount
      stopHeight = if (startHeight - 1 + batchSize > blockCount) blockCount else startHeight - 1 + batchSize
      stopBlockOpt <- getHeadersByHeight(stopHeight.toInt).map(_.headOption)
      stopBlock = stopBlockOpt.getOrElse(throw new RuntimeException(s"Unknown header height ${stopHeight}"))
    } yield {
      if (startHeight > stopHeight)
        None
      else
        Some((startHeight, stopBlock.hashBE.flip))
    }
  }

  override def processFilterHeaders(
      filterHeaders: Vector[FilterHeader],
      stopHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[ChainApi] = {

    val filterHeadersToCreateF = for {
      blockHeaders <- blockHeaderDAO.getNChildren(stopHash, filterHeaders.size - 1).map(_.sortBy(_.height))
    } yield {
      if (blockHeaders.size != filterHeaders.size) {
        throw new RuntimeException(s"Filter header batch size does not match block header batch size ${filterHeaders.size} != ${blockHeaders.size}")
      }
      blockHeaders.indices.toVector.map { i =>
        val blockHeader = blockHeaders(i)
        val filterHeader = filterHeaders(i)
        CompactFilterHeaderDbHelper.fromFilterHeader(
          filterHeader,
          blockHeader.hashBE,
          blockHeader.height)
      }
    }

    for {
      filterHeadersToCreate <- filterHeadersToCreateF
      _ <- if (filterHeadersToCreate.nonEmpty && filterHeadersToCreate.head.height > 0) {
        filterHeaderDAO.findByHash(filterHeadersToCreate.head.previousFilterHeaderBE).map { prevHeaderOpt =>
          require(prevHeaderOpt.nonEmpty, s"Previous filter header does not exist: ${filterHeadersToCreate.head.previousFilterHeaderBE}")
          require(prevHeaderOpt.get.height == filterHeadersToCreate.head.height - 1, s"Unexpected previous header's height: ${prevHeaderOpt.get.height} != ${filterHeadersToCreate.head.height - 1}")
        }
      } else Future.unit
      _ <- filterHeaderDAO.upsertAll(filterHeadersToCreate)
    } yield this
  }

  override def processFilters(messages: Vector[CompactFilterMessage])(implicit ec: ExecutionContext): Future[ChainApi] = {
    for {
      filterHeaders <- filterHeaderDAO.findAllByBlockHashes(messages.map(_.blockHash.flip)).map(_.sortBy(_.height))
      _ = if (filterHeaders.size != messages.size) {
        throw new RuntimeException(s"Filter batch size does not match filter header batch size ${messages.size} != ${filterHeaders.size}")
      }
      byBlockHash = messages.groupBy(_.blockHash.flip)
      compactFilterDbs = filterHeaders.map { filterHeader =>
        byBlockHash.get(filterHeader.blockHashBE) match {
          case Some(messages) if messages.size == 1 =>
            val message = messages.head
            val filterHashBE = CryptoUtil.doubleSHA256(message.filterBytes).flip
            if (filterHashBE != filterHeader.filterHashBE) {
              val errMsg = s"Filter hash does not match filter header hash: ${filterHashBE} != ${filterHeader.filterHashBE}\n" +
                s"filter=${message.filterBytes.toHex}\nblock hash=${message.blockHash}\nfilterHeader=${filterHeader}"
              logger.warn(errMsg)
            }
            val filter = CompactFilterDbHelper.fromFilterBytes(message.filterBytes, filterHeader.blockHashBE, filterHeader.height)
            filter
          case error@_ =>
            throw new RuntimeException()
        }
      }
      _ <- filterDAO.upsertAll(compactFilterDbs)
    } yield {
      this
    }
  }

  override def processCheckpoint(
      filterHeaderHash: DoubleSha256DigestBE,
      blockHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[ChainApi] = {
      blockFilterCheckpoints.get(blockHash) match {
        case Some(oldFilterHeaderHash) =>
          if (filterHeaderHash != oldFilterHeaderHash)
            Future.failed(new RuntimeException(
              "The peer sent us a different filter header hash"))
          else
            Future.successful(this.copy(
              blockFilterCheckpoints =
                blockFilterCheckpoints.updated(blockHash, filterHeaderHash)))
        case None =>
          Future.successful(this)
      }

  }

  override def getHighestFilterHeader(
      implicit ec: ExecutionContext): Future[Option[CompactFilterHeaderDb]] = {
    filterHeaderDAO.findHighest()
  }

  override def getFilterHeader(hash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[Option[FilterHeader]] = {
    filterHeaderDAO.findByHash(hash).map(_.map(x => x.filterHeader))
  }

  override def getHighestFilter(implicit ec: ExecutionContext): Future[Option[CompactFilterDb]] = {
    filterDAO.findHighest()
  }

  override def getFilter(blockHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[Option[CompactFilterDb]] = {
    filterDAO.findByBlockHash(blockHash)
  }

  override def getHeadersByHeight(height: Int)(
    implicit ec: ExecutionContext): Future[Seq[BlockHeaderDb]] = {
    blockHeaderDAO.findByHeight(height)
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
      implicit chainConfig: ChainAppConfig): ChainHandler = {
    new ChainHandler(blockHeaderDAO = blockHeaderDAO,
                     filterHeaderDAO = filterHeaderDAO,
                     filterDAO = filterDAO,
                     blockchains = Vector(blockchains),
                     blockFilterCheckpoints = Map.empty)
  }
}
