package org.bitcoins.wallet.internal

import org.bitcoins.core.api.chain.ChainQueryApi.{
  FilterResponse,
  InvalidBlockRange
}
import org.bitcoins.core.api.wallet.NeutrinoWalletApi.BlockMatchingResponse
import org.bitcoins.core.gcs.SimpleFilterMatcher
import org.bitcoins.core.hd.{HDAccount, HDChainType}
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.rescan.RescanState
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.wallet.{Wallet, WalletLogger}

import scala.concurrent.{ExecutionContext, Future}

private[wallet] trait RescanHandling extends WalletLogger {
  self: Wallet =>

  /////////////////////
  // Public facing API

  override def isRescanning(): Future[Boolean] = stateDescriptorDAO.isRescanning

  /** @inheritdoc */
  override def rescanNeutrinoWallet(
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      useCreationTime: Boolean,
      force: Boolean)(implicit ec: ExecutionContext): Future[RescanState] = {
    for {
      account <- getDefaultAccount()
      state <- rescanNeutrinoWallet(account.hdAccount,
                                    startOpt,
                                    endOpt,
                                    addressBatchSize,
                                    useCreationTime,
                                    force)
    } yield state
  }

  /** @inheritdoc */
  def rescanNeutrinoWallet(
      account: HDAccount,
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      useCreationTime: Boolean = true,
      force: Boolean = false): Future[RescanState] = {
    for {
      doRescan <-
        if (force) stateDescriptorDAO.updateRescanning(true).map(_.rescanning)
        else
          stateDescriptorDAO.compareAndSetRescanning(expectedValue = false,
                                                     newValue = true)
      rescanState <-
        if (doRescan) {
          logger.info(
            s"Starting rescanning the wallet from ${startOpt} to ${endOpt} useCreationTime=$useCreationTime")
          val startTime = System.currentTimeMillis()
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
            _ <- clearUtxos(account)
            _ <- doNeutrinoRescan(account, start, endOpt, addressBatchSize)
            _ <- stateDescriptorDAO.updateRescanning(false)
          } yield {
            logger.info(s"Finished rescanning the wallet. It took ${System
              .currentTimeMillis() - startTime}ms")
            RescanState.RescanDone
          }

          res.recoverWith { case err: Throwable =>
            logger.error(s"Failed to rescan wallet", err)
            stateDescriptorDAO
              .updateRescanning(false)
              .flatMap(_ => Future.failed(err))
          }

          res
        } else {
          logger.warn(
            s"Rescan already started, ignoring request to start another one")
          Future.successful(RescanState.RescanInProgress)
        }

    } yield rescanState
  }

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
      parallelismLevel: Int = Runtime.getRuntime.availableProcessors())(implicit
      ec: ExecutionContext): Future[Vector[BlockMatchingResponse]] = {
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
        endHeight <- endOpt.fold(chainQueryApi.getFilterCount())(
          chainQueryApi.getHeightByBlockStamp)
        _ = if (startHeight > endHeight)
          throw InvalidBlockRange(
            s"End position cannot precede start: $startHeight:$endHeight")
        _ = logger.info(
          s"Beginning to search for matches between ${startHeight}:${endHeight} against ${scripts.length} spks")
        range = startHeight.to(endHeight)
        matched <- FutureUtil.batchAndSyncExecute(
          elements = range.toVector,
          f = fetchFiltersInRange(scripts, parallelismLevel),
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
      addressCount <- addressDAO.count()
      _ <- matchBlocks(scriptPubKeys = scriptPubKeys,
                       endOpt = endOpt,
                       startOpt = startOpt)
      externalGap <- calcAddressGap(HDChainType.External, account)
      changeGap <- calcAddressGap(HDChainType.Change, account)
      res <- {
        logger.info(s"addressCount=$addressCount externalGap=$externalGap")
        if (addressCount != 0) {
          logger.info(
            s"We have a small number of addresses preloaded into the wallet")
          Future.unit
        } else if (
          externalGap >= walletConfig.addressGapLimit && changeGap >= walletConfig.addressGapLimit
        ) {
          logger.info(
            s"Did not find any funds within the last ${walletConfig.addressGapLimit} addresses. Stopping our rescan.")
          Future.unit
        } else {
          logger.info(
            s"Attempting rescan again with fresh pool of addresses as we had a " +
              s"match within our address gap limit of ${walletConfig.addressGapLimit}")
          doNeutrinoRescan(account, startOpt, endOpt, addressBatchSize)
        }
      }
    } yield res
  }

  private def calcAddressGap(
      chainType: HDChainType,
      account: HDAccount): Future[Int] = {
    for {
      addressDbs <- addressDAO.findAllForAccount(account)
      addressGap <-
        addressDbs
          //make sure all addressDb are of the correct chainType
          //and they are sorted according to their index so we can
          //calculate the gap accurately
          .filter(_.accountChain == chainType)
          .sortBy(_.addressIndex)
          .foldLeft(Future.successful(0)) { (prevNF, addressDb) =>
            for {
              prevN <- prevNF
              spendingInfoDbs <-
                spendingInfoDAO.findByScriptPubKeyId(addressDb.scriptPubKeyId)
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
    blocks.foldLeft(Future.unit) { (prevF, blockHash) =>
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

    val blocksF = for {
      blocks <- getMatchingBlocks(scripts = scriptPubKeys,
                                  startOpt = startOpt,
                                  endOpt = endOpt)(
        ExecutionContext.fromExecutor(walletConfig.rescanThreadPool))
    } yield {
      blocks.sortBy(_.blockHeight).map(_.blockHash.flip)
    }

    blocksF
  }

  /** Use to generate a list of addresses to search when restoring our wallet
    *  from our mneomnic seed
    */
  private def generateAddressesForRescan(
      account: HDAccount,
      count: Int): Future[Vector[BitcoinAddress]] = {
    val receiveAddressesF = 1
      .to(count)
      .foldLeft(Future.successful(Vector.empty[BitcoinAddress])) {
        (prevFuture, _) =>
          for {
            prev <- prevFuture
            address <- getNewAddress(account)
          } yield prev :+ address
      }

    val changeAddressesF = 1
      .to(count)
      .foldLeft(Future.successful(Vector.empty[BitcoinAddress])) {
        (prevFuture, _) =>
          for {
            prev <- prevFuture
            address <- getNewChangeAddress(account)
          } yield prev :+ address
      }
    for {
      receiveAddresses <- receiveAddressesF
      changeAddresses <- changeAddressesF
    } yield receiveAddresses ++ changeAddresses

  }

  private def generateScriptPubKeys(
      account: HDAccount,
      count: Int): Future[Vector[ScriptPubKey]] = {
    val addressCountF = addressDAO.count()
    for {
      addressCount <- addressCountF
      addresses <- {
        if (addressCount == 0) {
          generateAddressesForRescan(account, count)
        } else {
          //we don't want to continously generate addresses
          //if our wallet already has them, so just use what is in the
          //database already
          addressDAO.findAllAddresses().map(_.map(_.address))
        }
      }
      spksDb <- scriptPubKeyDAO.findAll()
    } yield {
      val addrSpks =
        addresses.map(_.scriptPubKey)
      val otherSpks = spksDb.map(_.scriptPubKey)

      (addrSpks ++ otherSpks).distinct
    }
  }

  private def fetchFiltersInRange(
      scripts: Vector[ScriptPubKey],
      parallelismLevel: Int)(heightRange: Vector[Int])(implicit
      ec: ExecutionContext): Future[Vector[BlockMatchingResponse]] = {
    val startHeight = heightRange.head
    val endHeight = heightRange.last
    for {
      filtersResponse <- chainQueryApi.getFiltersBetweenHeights(
        startHeight = startHeight,
        endHeight = endHeight)
      filtered <- findMatches(filtersResponse, scripts, parallelismLevel)
      _ <- downloadAndProcessBlocks(filtered.map(_.blockHash.flip))
    } yield {
      logger.info(
        s"Found ${filtered.length} matches from start=$startHeight to end=$endHeight")
      filtered
    }
  }

  private[wallet] def findMatches(
      filters: Vector[FilterResponse],
      scripts: Vector[ScriptPubKey],
      parallelismLevel: Int)(implicit
      ec: ExecutionContext): Future[Vector[BlockMatchingResponse]] = {
    if (filters.isEmpty) {
      logger.info("No Filters to check against")
      Future.successful(Vector.empty)
    } else if (scripts.isEmpty) {
      logger.info("No scripts to check against")
      Future.successful(Vector.empty)
    } else {
      val bytes = scripts.map(_.asmBytes)
      /* Iterates over the grouped vector of filters to find matches with the given [[bytes]]. */
      val groupSize = calcGroupSize(filters.size, parallelismLevel)
      val filterGroups = filters.grouped(groupSize).toVector
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
                    logger.info(s"Found a match in block ${filter.blockHeight}")
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
    * It's needed to limit number of threads required to run the matching
    */
  private def calcGroupSize(vectorSize: Int, parallelismLevel: Int): Int = {
    if (vectorSize / parallelismLevel * parallelismLevel < vectorSize)
      vectorSize / parallelismLevel + 1
    else vectorSize / parallelismLevel
  }
}
