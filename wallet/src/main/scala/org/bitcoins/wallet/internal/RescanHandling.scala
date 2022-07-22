package org.bitcoins.wallet.internal

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Keep, Merge, Sink, Source}
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
import org.bitcoins.core.wallet.rescan.RescanState
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.wallet.{Wallet, WalletLogger}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

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
            state <- doNeutrinoRescan(account, start, endOpt, addressBatchSize)
            _ <- stateDescriptorDAO.updateRescanning(false)
            _ <- walletCallbacks.executeOnRescanComplete(logger)
          } yield {
            logger.info(s"Finished rescanning the wallet. It took ${System
              .currentTimeMillis() - startTime}ms")

            state
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
          Future.successful(RescanState.RescanAlreadyStarted)
        }

    } yield rescanState
  }

  lazy val walletCreationBlockHeight: Future[BlockHeight] =
    chainQueryApi
      .epochSecondToBlockHeight(creationTime.getEpochSecond)
      .map(BlockHeight)

  private def buildFilterMatchFlow(
      range: Range,
      scripts: Vector[ScriptPubKey],
      parallelism: Int,
      batchSize: Int)(implicit
      system: ActorSystem): RescanState.RescanStarted = {
    val maybe = Source.maybe[Int]
    val combine: Source[Int, Promise[Option[Int]]] = {
      Source.combineMat(maybe, Source(range))(Merge(_))(Keep.left)
    }
    val seed: Int => Vector[Int] = { case int =>
      Vector(int)
    }
    val aggregate: (Vector[Int], Int) => Vector[Int] = {
      case (vec: Vector[Int], int: Int) => vec.appended(int)
    }

    //this promise is completed after we scan the last filter
    //in the rescanSink
    val rescanCompletePromise: Promise[Unit] = Promise()

    //fetches filters, matches filters against our wallet, and then request blocks
    //for the wallet to process
    val rescanSink: Sink[Int, Future[Seq[Vector[BlockMatchingResponse]]]] = {
      Flow[Int]
        .batch[Vector[Int]](batchSize, seed)(aggregate)
        .mapAsync(1) { case heightRange =>
          val f = fetchFiltersInRange(scripts, parallelism)(heightRange)(
            ExecutionContext.fromExecutor(walletConfig.rescanThreadPool))

          f.onComplete {
            case Success(_) =>
              if (heightRange.lastOption == range.lastOption) {
                //complete the stream if we processed the last filter
                rescanCompletePromise.success(())
              }
            case Failure(_) => //do nothing, the stream will fail on its own
          }
          f
        }
        .toMat(Sink.seq)(Keep.right)
    }

    //the materialized values of the two streams
    //completeRescanEarly allows us to safely complete the rescan early
    //matchingBlocksF is materialized when the stream is complete. This is all blocks our wallet matched
    val (completeRescanEarlyP, matchingBlocksF) =
      combine.toMat(rescanSink)(Keep.both).run()

    //if we have seen the last filter, complete the rescanEarlyP so we are consistent
    rescanCompletePromise.future.map(_ => completeRescanEarlyP.success(None))

    val flatten = matchingBlocksF.map(_.flatten.toVector)

    //return RescanStarted with access to the ability to complete the rescan early
    //via the completeRescanEarlyP promise.
    RescanState.RescanStarted(completeRescanEarlyP, flatten)
  }

  /** Iterates over the block filters in order to find filters that match to the given addresses
    *
    * I queries the filter database for [[batchSize]] filters a time
    * and tries to run [[GolombFilter.matchesAny]] for each filter.
    *
    * It tries to match the filters in parallel using [[parallelismLevel]] threads.
    * For best results use it with a separate execution context.
    *
    * @param scripts list of [[ScriptPubKey]]'s to watch
    * @param startOpt start point (if empty it starts with the genesis block)
    * @param endOpt end point (if empty it ends with the best tip)
    * @param batchSize number of filters that can be matched in one batch
    * @param parallelismLevel max number of threads required to perform matching
    *                         (default [[Runtime.getRuntime.availableProcessors()]])
    * @return a list of matching block hashes
    */
  def getMatchingBlocks(
      scripts: Vector[ScriptPubKey],
      startOpt: Option[BlockStamp] = None,
      endOpt: Option[BlockStamp] = None,
      batchSize: Int = 100,
      parallelismLevel: Int = Runtime.getRuntime.availableProcessors())(implicit
      ec: ExecutionContext): Future[RescanState] = {
    require(batchSize > 0, "batch size must be greater than zero")
    require(parallelismLevel > 0, "parallelism level must be greater than zero")
    implicit val system: ActorSystem = ActorSystem(
      s"getMatchingBlocks-${System.currentTimeMillis()}")
    if (scripts.isEmpty) {
      Future.successful(RescanState.RescanDone)
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

        rescanStarted = buildFilterMatchFlow(range,
                                             scripts,
                                             parallelismLevel,
                                             batchSize)
      } yield {
        rescanStarted
      }
    }
  }

  /////////////////////
  // Private methods

  private def doNeutrinoRescan(
      account: HDAccount,
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int): Future[RescanState] = {
    for {
      scriptPubKeys <- generateScriptPubKeys(account, addressBatchSize)
      addressCount <- addressDAO.count()
      inProgress <- matchBlocks(scriptPubKeys = scriptPubKeys,
                                endOpt = endOpt,
                                startOpt = startOpt)
      externalGap <- calcAddressGap(HDChainType.External, account)
      changeGap <- calcAddressGap(HDChainType.Change, account)
      _ <- {
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
    } yield inProgress
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
      startOpt: Option[BlockStamp]): Future[RescanState] = {

    val rescanStateF = for {
      rescanState <- getMatchingBlocks(scripts = scriptPubKeys,
                                       startOpt = startOpt,
                                       endOpt = endOpt)(
        ExecutionContext.fromExecutor(walletConfig.rescanThreadPool))
    } yield {
      rescanState
    }

    rescanStateF
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
    logger.info(s"Searching filters from start=$startHeight to end=$endHeight")
    for {
      filtersResponse <- chainQueryApi.getFiltersBetweenHeights(
        startHeight = startHeight,
        endHeight = endHeight)
      filtered <- findMatches(filtersResponse, scripts, parallelismLevel)(ec)
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
