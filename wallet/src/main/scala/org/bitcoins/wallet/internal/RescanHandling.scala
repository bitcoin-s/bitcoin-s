package org.bitcoins.wallet.internal

import java.util.concurrent.Executors

import org.bitcoins.core.api.ChainQueryApi.{FilterResponse, InvalidBlockRange}
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.SimpleFilterMatcher
import org.bitcoins.core.hd.{HDAccount, HDChainType}
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.wallet.api.WalletApi.BlockMatchingResponse
import org.bitcoins.wallet.{Wallet, WalletLogger}

import scala.concurrent.{ExecutionContext, Future}

private[wallet] trait RescanHandling extends WalletLogger {
  self: Wallet =>

  /////////////////////
  // Public facing API

  /** @inheritdoc */
  override def rescanNeutrinoWallet(
      account: HDAccount,
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      useCreationTime: Boolean = true): Future[Unit] = {

    logger.info(s"Starting rescanning the wallet from ${startOpt} to ${endOpt}")

    val res = for {
      start <- (startOpt, useCreationTime) match {
        case (Some(_), true) =>
          Future.failed(new IllegalArgumentException(
            "Cannot define a starting block and use the wallet creation time"))
        case (Some(value), false) =>
          Future.successful(Some(value))
        case (None, true) =>
          walletCreationBlockHeight.map(Some(_))
        case (None, false) =>
          Future.successful(None)
      }
      _ <- clearUtxosAndAddresses(account)
      _ <- doNeutrinoRescan(account, start, endOpt, addressBatchSize)
    } yield ()

    res.onComplete(_ => logger.info("Finished rescanning the wallet"))

    res
  }

  /** @inheritdoc */
  override def rescanSPVWallet(): Future[Unit] =
    Future.failed(new RuntimeException("Rescan not implemented for SPV wallet"))

  lazy val walletCreationBlockHeight: Future[BlockHeight] =
    chainQueryApi
      .epochSecondToBlockHeight(creationTime.getEpochSecond)
      .map(BlockHeight)

  /** @inheritdoc */
  override def getMatchingBlocks(
      scripts: Vector[ScriptPubKey],
      startOpt: Option[BlockStamp] = None,
      endOpt: Option[BlockStamp] = None,
      batchSize: Int = 100,
      parallelismLevel: Int = Runtime.getRuntime.availableProcessors())(
      implicit ec: ExecutionContext): Future[Vector[BlockMatchingResponse]] = {
    require(batchSize > 0, "batch size must be greater than zero")
    require(parallelismLevel > 0, "parallelism level must be greater than zero")
    if (scripts.isEmpty) {
      Future.successful(Vector.empty)
    } else {

      for {
        startHeight <- startOpt.fold(Future.successful(0))(
          chainQueryApi.getHeightByBlockStamp)
        _ = if (startHeight < 0)
          throw InvalidBlockRange(s"Start position cannot negative")
        endHeight <- endOpt.fold(chainQueryApi.getFilterCount)(
          chainQueryApi.getHeightByBlockStamp)
        _ = if (startHeight > endHeight)
          throw InvalidBlockRange(
            s"End position cannot precede start: $startHeight:$endHeight")
        _ = logger.info(
          s"Beginning to search for matches between ${startHeight}:${endHeight} against ${scripts.length} spks")
        range = startHeight.to(endHeight)
        matched <- FutureUtil.batchExecute(
          elements = range.toVector,
          f = fetchFiltersInRange(scripts, parallelismLevel),
          init = Vector.empty,
          batchSize = batchSize)
      } yield {
        logger.info(s"Matched ${matched.length} blocks on rescan")
        matched
      }
    }
  }

  /////////////////////
  // Private methods

  private def doNeutrinoRescan(
      account: HDAccount,
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int): Future[Unit] = {
    for {
      scriptPubKeys <- generateScriptPubKeys(account, addressBatchSize)
      blocks <- matchBlocks(scriptPubKeys = scriptPubKeys,
                            endOpt = endOpt,
                            startOpt = startOpt)
      _ <- downloadAndProcessBlocks(blocks)
      externalGap <- calcAddressGap(HDChainType.External)
      changeGap <- calcAddressGap(HDChainType.Change)
      res <- if (externalGap >= walletConfig.addressGapLimit && changeGap >= walletConfig.addressGapLimit) {
        pruneUnusedAddresses()
      } else {
        logger.info(
          s"Attempting rescan again with fresh pool of addresses as we had a " +
            s"match within our address gap limit of ${walletConfig.addressGapLimit}")
        doNeutrinoRescan(account, startOpt, endOpt, addressBatchSize)
      }
    } yield res
  }

  private def pruneUnusedAddresses(): Future[Unit] = {
    for {
      addressDbs <- addressDAO.findAll()
      _ <- addressDbs.foldLeft(FutureUtil.unit) { (prevF, addressDb) =>
        for {
          _ <- prevF
          spendingInfoDbs <- spendingInfoDAO.findByScriptPubKey(
            addressDb.scriptPubKey)
          _ <- if (spendingInfoDbs.isEmpty) addressDAO.delete(addressDb)
          else FutureUtil.unit
        } yield ()
      }
    } yield ()
  }

  private def calcAddressGap(chainType: HDChainType): Future[Int] = {
    for {
      addressDbs <- addressDAO.findAll()
      addressGap <- addressDbs
      //make sure all addressDb are of the correct chainType
      //and they are sorted according to their index so we can
      //calculate the gap accurately
        .filter(_.path.chain.chainType == chainType)
        .sortBy(_.path.address.index)
        .foldLeft(Future.successful(0)) { (prevNF, addressDb) =>
          for {
            prevN <- prevNF
            spendingInfoDbs <- spendingInfoDAO.findByScriptPubKey(
              addressDb.scriptPubKey)
          } yield {
            if (spendingInfoDbs.isEmpty) prevN + 1 else 0
          }
        }
    } yield {
      logger.debug(s"Address gap: $addressGap")
      addressGap
    }
  }

  private def downloadAndProcessBlocks(
      blocks: Vector[DoubleSha256Digest]): Future[Unit] = {
    logger.info(s"Requesting ${blocks.size} block(s)")
    blocks.foldLeft(FutureUtil.unit) { (prevF, blockHash) =>
      val completedF = subscribeForBlockProcessingCompletionSignal(blockHash)
      for {
        _ <- prevF
        _ <- nodeApi.downloadBlocks(Vector(blockHash))
        _ <- completedF
      } yield ()
    }
  }

  private def matchBlocks(
      scriptPubKeys: Vector[ScriptPubKey],
      endOpt: Option[BlockStamp],
      startOpt: Option[BlockStamp]): Future[Vector[DoubleSha256Digest]] = {
    val threadPool =
      Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors() * 2)

    val blocksF = for {
      blocks <- getMatchingBlocks(
        scripts = scriptPubKeys,
        startOpt = startOpt,
        endOpt = endOpt)(ExecutionContext.fromExecutor(threadPool))
    } yield {
      blocks.sortBy(_.blockHeight).map(_.blockHash.flip)
    }

    blocksF.onComplete(_ => threadPool.shutdown())
    blocksF
  }

  private def generateScriptPubKeys(
      account: HDAccount,
      count: Int): Future[Vector[ScriptPubKey]] = {
    for {
      addresses <- 1
        .to(count)
        .foldLeft(Future.successful(Vector.empty[BitcoinAddress])) {
          (prevFuture, _) =>
            for {
              prev <- prevFuture
              address <- getNewAddress(account)
            } yield prev :+ address
        }
      changeAddresses <- 1
        .to(count)
        .foldLeft(Future.successful(Vector.empty[BitcoinAddress])) {
          (prevFuture, _) =>
            for {
              prev <- prevFuture
              address <- getNewChangeAddress(account)
            } yield prev :+ address
        }
    } yield addresses.map(_.scriptPubKey) ++ changeAddresses.map(_.scriptPubKey)
  }

  private def fetchFiltersInRange(
      scripts: Vector[ScriptPubKey],
      parallelismLevel: Int)(
      heightRange: Vector[Int]): Future[Vector[BlockMatchingResponse]] = {
    val startHeight = heightRange.head
    val endHeight = heightRange.last
    for {
      filtersResponse <- chainQueryApi.getFiltersBetweenHeights(
        startHeight = startHeight,
        endHeight = endHeight)
      filtered <- findMatches(filtersResponse, scripts, parallelismLevel)
    } yield filtered.toVector
  }

  private def findMatches(
      filters: Vector[FilterResponse],
      scripts: Vector[ScriptPubKey],
      parallelismLevel: Int): Future[Iterator[BlockMatchingResponse]] = {
    if (filters.isEmpty)
      Future.successful(Iterator.empty)
    else {
      val bytes = scripts.map(_.asmBytes)
      /* Iterates over the grouped vector of filters to find matches with the given [[bytes]]. */
      val groupSize = calcGroupSize(filters.size, parallelismLevel)
      val filterGroups = filters.grouped(groupSize)
      // Sequence on the filter groups making sure the number of threads doesn't exceed [[parallelismLevel]].
      Future
        .sequence(filterGroups.map { filterGroup =>
          // We need to wrap in a future here to make sure we can
          // potentially run these matches in parallel
          Future {
            // Find any matches in the group and add the corresponding block hashes into the result
            filterGroup
              .foldLeft(Vector.empty[BlockMatchingResponse]) {
                (blocks, filter) =>
                  val matcher = SimpleFilterMatcher(filter.compactFilter)
                  if (matcher.matchesAny(bytes)) {
                    blocks :+ BlockMatchingResponse(filter.blockHash,
                                                    filter.blockHeight)
                  } else {
                    blocks
                  }
              }
          }
        })
        .map(_.flatten)
    }
  }

  /** Calculates group size to split a filter vector into [[parallelismLevel]] groups.
    * It's needed to limit number of threads required to run the matching */
  private def calcGroupSize(vectorSize: Int, parallelismLevel: Int): Int = {
    if (vectorSize / parallelismLevel * parallelismLevel < vectorSize)
      vectorSize / parallelismLevel + 1
    else vectorSize / parallelismLevel
  }

}
