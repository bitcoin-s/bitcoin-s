package org.bitcoins.chain.blockchain

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb, CompactFilterHeaderDAO, CompactFilterHeaderDbHelper}
import org.bitcoins.chain.validation.TipUpdateResult
import org.bitcoins.chain.validation.TipUpdateResult.{BadNonce, BadPOW, BadPreviousBlockHash}
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.gcs.{FilterHeader, GolombFilter}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db.ChainVerificationLogger
import org.sqlite.{SQLiteErrorCode, SQLiteException}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Chain Handler is meant to be the reference implementation
  * of [[org.bitcoins.chain.api.ChainApi ChainApi]], this is the entry point in to the
  * chain project.
  */
case class ChainHandler(
    blockHeaderDAO: BlockHeaderDAO,
    filterHeaderDAO: CompactFilterHeaderDAO,
    blockchains: Vector[Blockchain],
    blockFilters: Map[DoubleSha256DigestBE, GolombFilter],
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

  override def processHeader(header: BlockHeader)(
      implicit ec: ExecutionContext): Future[ChainHandler] = {
    logger.debug(
      s"Processing header=${header.hashBE.hex}, previousHash=${header.previousBlockHashBE.hex}")

    val blockchainUpdate =
      Blockchain.connectTip(header = header, blockchains = blockchains)

    val newHandlerF = blockchainUpdate match {
      case BlockchainUpdate.Successful(newChain, updatedHeader) =>
        //now we have successfully connected the header, we need to insert
        //it into the database
        val createdF = blockHeaderDAO.create(updatedHeader)
        createdF.map { header =>
          logger.debug(
            s"Connected new header to blockchain, height=${header.height} hash=${header.hashBE}")
          val chainIdxOpt = blockchains.zipWithIndex.find {
            case (chain, _) =>
              val oldTip = newChain(1) //should be safe, even with genesis header as we just connected a tip
              oldTip == chain.tip
          }

          val updatedChains = {
            chainIdxOpt match {
              case Some((_, idx)) =>
                logger.trace(
                  s"Updating chain at idx=${idx} out of competing chains=${blockchains.length} with new tip=${header.hashBE.hex}")
                blockchains.updated(idx, newChain)

              case None =>
                logger.info(
                  s"New competing blockchain with tip=${newChain.tip}")
                blockchains.:+(newChain)
            }
          }

          this.copy(blockchains = updatedChains)
        }
      case BlockchainUpdate.Failed(_, _, reason) =>
        val errMsg =
          s"Failed to add header to chain, header=${header.hashBE.hex} reason=${reason}"
        logger.warn(errMsg)
        // potential chain split happening, let's log what's going on
        logTipConnectionFailure(reason).flatMap { _ =>
          Future.failed(new RuntimeException(errMsg))
        }
    }

    newHandlerF
  }

  /** Logs a tip connection failure by querying local chain state
    * and comparing it to the received `TipUpdateResult`
    */
  private def logTipConnectionFailure(failure: TipUpdateResult.Failure)(
      implicit ec: ExecutionContext): Future[Unit] = {
    failure match {
      case _ @(_: BadPOW | _: BadNonce) =>
        // TODO: Log this in a meaningful way
        FutureUtil.unit
      case _: BadPreviousBlockHash =>
        blockHeaderDAO.chainTips.map { tips =>
          if (tips.length > 1) {
            logger.warn {
              s"We have multiple (${tips.length}) , competing chainTips=${tips
                .map(_.hashBE.hex)
                .mkString("[", ",", "]")}"
            }
          } else {
            logger.warn(
              s"We don't have competing chainTips. Most recent, valid header=${tips.head.hashBE.hex}")
          }
        }
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

  override def processFilterHeader(filterHeader: FilterHeader, blockHash: DoubleSha256DigestBE, height: Int)(implicit ec: ExecutionContext): Future[ChainApi] = {
    val filterHeaderDb = CompactFilterHeaderDbHelper.fromFilterHeader(filterHeader, blockHash, height)
    for {
      _ <- filterHeaderDAO.create(filterHeaderDb).recoverWith {
        case e: SQLiteException if e.getResultCode == SQLiteErrorCode.SQLITE_CONSTRAINT =>
          filterHeaderDAO.findByHash(filterHeaderDb.hashBE).map {
            case Some(fhDb) =>
              if (fhDb != filterHeaderDb)
                throw new RuntimeException(s"We have a conflicting compact filter header (${filterHeaderDb.hashBE}) in the DB")
              else
                fhDb
            case None => throw new RuntimeException("Something is really wrong with cfheader table")
          }
      }
    } yield this
  }

  override def processFilter(golombFilter: GolombFilter, blockHash: DoubleSha256DigestBE)(implicit ec: ExecutionContext): Future[ChainApi] = {
    for {
      filterHeaderOpt <- filterHeaderDAO.findByBlockHash(blockHash)
      filterHeader = filterHeaderOpt.getOrElse(throw new RuntimeException(s"Cannot find a filter header for block hash ${blockHash}"))
    } yield {
      logger.debug(golombFilter.hash.hex)
      logger.debug(filterHeader.hashBE.hex)
      logger.debug(filterHeader.hashBE.flip.hex)
      if (golombFilter.hash != filterHeader.hashBE.flip) {
        logger.error("Filter hash does not match filter header hash")
      }
      this.copy(blockFilters = blockFilters.updated(blockHash, golombFilter))
    }
  }

  override def processCheckpoint(filterHeaderHash: DoubleSha256DigestBE, blockHash: DoubleSha256DigestBE)(implicit ec: ExecutionContext): Future[ChainApi] = {
    Future {
      blockFilterCheckpoints.get(blockHash) match {
        case Some(oldFilterHeaderHash) =>
          if (filterHeaderHash != oldFilterHeaderHash)
            throw new RuntimeException("The peer sent us a different filter header hash")
          else
            this.copy(blockFilterCheckpoints = blockFilterCheckpoints.updated(blockHash, filterHeaderHash))
        case None =>
          this
      }
    }
  }
}

object ChainHandler {

  /** Constructs a [[ChainHandler chain handler]] from the state in the database
    * This gives us the guaranteed latest state we have in the database
    * */
  def fromDatabase(blockHeaderDAO: BlockHeaderDAO, filterHeaderDAO: CompactFilterHeaderDAO)(
      implicit ec: ExecutionContext,
      chainConfig: ChainAppConfig): Future[ChainHandler] = {
    val bestChainsF = blockHeaderDAO.getBlockchains()

    bestChainsF.map(chains =>
      new ChainHandler(blockHeaderDAO = blockHeaderDAO, filterHeaderDAO = filterHeaderDAO, blockchains = chains, blockFilters = Map.empty, blockFilterCheckpoints = Map.empty))
  }

  def apply(blockHeaderDAO: BlockHeaderDAO, filterHeaderDAO: CompactFilterHeaderDAO, blockchains: Blockchain)(
      implicit chainConfig: ChainAppConfig): ChainHandler = {
    new ChainHandler(blockHeaderDAO, filterHeaderDAO, Vector(blockchains), blockFilters = Map.empty, blockFilterCheckpoints = Map.empty)
  }
}
