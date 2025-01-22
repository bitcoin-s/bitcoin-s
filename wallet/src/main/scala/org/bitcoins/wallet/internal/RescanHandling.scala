package org.bitcoins.wallet.internal

import org.apache.pekko.NotUsed
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.scaladsl.{Flow, Keep, Merge, Sink, Source}
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.chain.ChainQueryApi.{
  FilterResponse,
  InvalidBlockRange
}
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.NeutrinoWalletApi.BlockMatchingResponse
import org.bitcoins.core.api.wallet.{
  AccountHandlingApi,
  AddressHandlingApi,
  RescanHandlingApi,
  TransactionProcessingApi
}
import org.bitcoins.core.gcs.SimpleFilterMatcher
import org.bitcoins.core.hd.{HDAccount, HDChainType}
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.rescan.RescanState
import org.bitcoins.core.wallet.rescan.RescanState.RescanTerminatedEarly
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.wallet.callback.WalletCallbacks
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{
  AddressDAO,
  SpendingInfoDAO,
  WalletDAOs,
  WalletStateDescriptorDAO
}
import org.bitcoins.wallet.WalletLogger

import java.time.Instant
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

case class RescanHandling(
    transactionProcessing: TransactionProcessingApi,
    accountHandling: AccountHandlingApi,
    addressHandling: AddressHandlingApi,
    chainQueryApi: ChainQueryApi,
    nodeApi: NodeApi,
    walletDAOs: WalletDAOs)(implicit
    walletConfig: WalletAppConfig,
    system: ActorSystem)
    extends RescanHandlingApi
    with WalletLogger {
  import system.dispatcher
  private def walletCallbacks: WalletCallbacks = walletConfig.callBacks
  private val stateDescriptorDAO: WalletStateDescriptorDAO =
    walletDAOs.stateDescriptorDAO
  private val addressDAO: AddressDAO = walletDAOs.addressDAO
  private val spendingInfoDAO: SpendingInfoDAO = walletDAOs.utxoDAO
  private val creationTime: Instant = walletConfig.creationTime

  override def isRescanning(): Future[Boolean] = stateDescriptorDAO.isRescanning

  /** @inheritdoc */
  override def rescanNeutrinoWallet(
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      useCreationTime: Boolean,
      force: Boolean
  ): Future[RescanState] = {
    for {
      account <- accountHandling.getDefaultAccount()
      state <- rescanNeutrinoWallet(
        account.hdAccount,
        startOpt,
        endOpt,
        addressBatchSize,
        useCreationTime,
        force
      )
    } yield state
  }

  /** @inheritdoc */
  def rescanNeutrinoWallet(
      account: HDAccount,
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      useCreationTime: Boolean = true,
      force: Boolean = false
  ): Future[RescanState] = {
    for {
      doRescan <-
        if (force) stateDescriptorDAO.updateRescanning(true).map(_.rescanning)
        else
          stateDescriptorDAO.compareAndSetRescanning(
            expectedValue = false,
            newValue = true
          )
      rescanState <-
        if (doRescan) {
          logger.info(
            s"Starting rescanning the wallet=${walletConfig.walletName} from ${startOpt} to ${endOpt} useCreationTime=$useCreationTime"
          )
          val startTime = System.currentTimeMillis()
          val resF: Future[RescanState] = for {
            start <- (startOpt, useCreationTime) match {
              case (Some(_), true) =>
                Future.failed(
                  new IllegalArgumentException(
                    "Cannot define a starting block and use the wallet creation time"
                  )
                )
              case (Some(value), false) =>
                Future.successful(Some(value))
              case (None, true) =>
                walletCreationBlockHeight.map(Some(_))
              case (None, false) =>
                Future.successful(None)
            }
            _ <- accountHandling.clearUtxos(account)
            state <- doNeutrinoRescan(
              account = account,
              startOpt = start,
              endOpt = endOpt,
              addressBatchSize = addressBatchSize,
              forceGenerateSpks = false
            )
            // purposefully don't map on this Future as it won't be completed until
            // the rescan is completely done.
            _ = RescanState.awaitRescanComplete(state).map { _ =>
              logger
                .info(s"Rescan is complete, resetting rescan state to false")
              val f = for {
                _ <- stateDescriptorDAO.updateRescanning(false)
                _ <- walletCallbacks
                  .executeOnRescanComplete(walletConfig.walletName)
              } yield ()

              f.failed.foreach(err =>
                logger.error(s"Failed to reset rescan state", err))
            }
          } yield {
            state
          }

          // register callbacks for resetting rescan flag in case of failure
          val _ = handleRescanFailure(resF)

          resF.map {
            case r: RescanState.RescanStarted =>
              r.entireRescanDoneF.map(_ =>
                logger.info(s"Finished rescanning the wallet. It took ${System
                    .currentTimeMillis() - startTime}ms"))
            case RescanState.RescanDone | RescanState.RescanAlreadyStarted |
                RescanState.RescanNotNeeded =>
            // nothing to log
          }

          resF
        } else {
          logger.warn(
            s"Rescan already started for wallet=${walletConfig.walletName}, ignoring request to start another one"
          )
          Future.successful(RescanState.RescanAlreadyStarted)
        }

    } yield {
      rescanState
    }
  }

  override def discoveryBatchSize(): Int = walletConfig.discoveryBatchSize

  /** Register callbacks to reset rescan flag in the database if there is a
    * rescan failure
    */
  private def handleRescanFailure(
      rescanStateF: Future[RescanState]
  ): Future[Unit] = {
    // handle the case where there is a top level rescan failure when _starting_ the rescan
    rescanStateF.recoverWith { case err: Throwable =>
      logger.error(
        s"Failed to rescan wallet1=${walletConfig.walletName} err=${err.getMessage}")
      stateDescriptorDAO
        .updateRescanning(false)
        .flatMap(_ => Future.failed(err))
    }

    // handle the case where the rescan fails while the rescan is in progress
    for {
      rescanState <- rescanStateF
      _ <- RescanState.awaitRescanDone(rescanState).recoverWith {
        case RescanTerminatedEarly =>
          logger.info(
            s"Rescan terminated early, don't reset isRescanning flag as new wallet is likely being loaded"
          )
          Future.unit
        case err: Throwable =>
          logger
            .error(
              s"Failed to rescan wallet=${walletConfig.walletName} err=${err.getMessage}")
          stateDescriptorDAO
            .updateRescanning(false)
            .flatMap(_ => Future.failed(err))

      }
    } yield ()
  }

  private lazy val walletCreationBlockHeight: Future[BlockHeight] =
    chainQueryApi
      .epochSecondToBlockHeight(creationTime.getEpochSecond)
      .map(BlockHeight.apply)

  private def buildRescanFlow(
      account: HDAccount,
      addressBatchSize: Int,
      range: Range,
      parallelism: Int,
      filterBatchSize: Int,
      forceGenerateSpks: Boolean
  ): RescanState.RescanStarted = {
    val scriptsF = accountHandling.generateScriptPubKeys(
      account = account,
      addressBatchSize = addressBatchSize,
      forceGenerateSpks = forceGenerateSpks
    )
    // by completing the promise returned by this sink
    // we will be able to arbitrarily terminate the stream
    // see: https://doc.akka.io/docs/akka/current/stream/operators/Source/maybe.html
    val maybe = Source.maybe[Int]

    // combine the Source.maybe with the Source providing filter heights
    // this is needed so we can arbitrarily kill the stream with
    // the promise returned by Source.maybe
    val combine: Source[Int, Promise[Option[Int]]] = {
      Source.combineMat(maybe, Source(range))(Merge(_))(Keep.left)
    }

    val seed: Int => Vector[Int] = { case int =>
      Vector(int)
    }
    val aggregate: (Vector[Int], Int) => Vector[Int] = {
      case (vec: Vector[Int], int: Int) => vec.:+(int)
    }

    // this promise is completed after we scan the last filter
    // in the rescanSink
    val rescanCompletePromise: Promise[Unit] = Promise()

    // fetches filters, matches filters against our wallet, and then request blocks
    // for the wallet to process. This sink takes as input filter heights
    // to fetch for rescanning.
    val rescanSink: Sink[Int, Future[Seq[Vector[BlockMatchingResponse]]]] = {
      Flow[Int]
        .batch[Vector[Int]](filterBatchSize, seed)(aggregate)
        .via(fetchFiltersFlow)
        .mapAsync(1) { filterResponse =>
          val heightRange = filterResponse.map(_.blockHeight)
          val f =
            scriptsF.flatMap { scripts =>
              searchFiltersForMatches(scripts, filterResponse, parallelism)
            }

          f.onComplete {
            case Success(_) =>
              if (heightRange.lastOption == range.lastOption) {
                // complete the stream if we processed the last filter
                rescanCompletePromise.success(())
              }
            case Failure(err) =>
              // do nothing, the stream will fail on its own
              logger.error(s"Failed to search filters for matches", err)
          }
          f
        }
        .toMat(Sink.seq)(Keep.right)
    }

    // the materialized values of the two streams
    // completeRescanEarly allows us to safely complete the rescan early
    // matchingBlocksF is materialized when the stream is complete. This is all blocks our wallet matched
    val (completeRescanEarlyP, matchingBlocksF) =
      combine.toMat(rescanSink)(Keep.both).run()

    val recursiveRescanP: Promise[RescanState] = Promise()

    // if we have seen the last filter, complete the rescanEarlyP so we are consistent
    rescanCompletePromise.future.map { _ =>
      completeRescanEarlyP.success(None)
    }

    val flatten = matchingBlocksF.map(_.flatten.toVector)

    // return RescanStarted with access to the ability to complete the rescan early
    // via the completeRescanEarlyP promise.
    RescanState.RescanStarted(completeRescanEarlyP, flatten, recursiveRescanP)
  }

  /** Iterates over the block filters in order to find filters that match to the
    * given addresses
    *
    * I queries the filter database for [[batchSize]] filters a time and tries
    * to run [[GolombFilter.matchesAny]] for each filter.
    *
    * It tries to match the filters in parallel using [[parallelismLevel]]
    * threads. For best results use it with a separate execution context.
    *
    * @param scripts
    *   list of [[ScriptPubKey]]'s to watch
    * @param startOpt
    *   start point (if empty it starts with the genesis block)
    * @param endOpt
    *   end point (if empty it ends with the best tip)
    * @param batchSize
    *   number of filters that can be matched in one batch
    * @param parallelismLevel
    *   max number of threads required to perform matching (default
    *   [[Runtime.getRuntime.availableProcessors()]])
    * @return
    *   a list of matching block hashes
    */
  private def getMatchingBlocks(
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      account: HDAccount,
      forceGenerateSpks: Boolean,
      parallelismLevel: Int = Runtime.getRuntime.availableProcessors()
  ): Future[RescanState.RescanStarted] = {
    require(addressBatchSize > 0, "batch size must be greater than zero")
    require(parallelismLevel > 0, "parallelism level must be greater than zero")
    for {
      startHeight <- startOpt.fold(Future.successful(0))(
        chainQueryApi.getHeightByBlockStamp
      )
      _ = if (startHeight < 0)
        throw InvalidBlockRange(s"Start position cannot negative")
      endHeight <- endOpt.fold(chainQueryApi.getFilterCount())(
        chainQueryApi.getHeightByBlockStamp
      )
      _ = if (startHeight > endHeight) {
        throw InvalidBlockRange(
          s"End position cannot precede start: $startHeight:$endHeight"
        )
      }
      _ = logger.info(
        s"Beginning to search for matches between ${startHeight}:${endHeight}"
      )
      range = startHeight.to(endHeight)

      rescanStarted = buildRescanFlow(
        account = account,
        addressBatchSize = addressBatchSize,
        range = range,
        parallelism = parallelismLevel,
        filterBatchSize = addressBatchSize,
        forceGenerateSpks = forceGenerateSpks
      )
    } yield {
      rescanStarted
    }
  }

  /////////////////////
  // Private methods

  private def doNeutrinoRescan(
      account: HDAccount,
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      forceGenerateSpks: Boolean
  ): Future[RescanState] = {
    for {
      inProgress <- matchBlocks(
        endOpt = endOpt,
        startOpt = startOpt,
        account = account,
        addressBatchSize = addressBatchSize,
        forceGenerateSpks
      )
      _ = recursiveRescan(
        prevState = inProgress,
        startOpt = startOpt,
        endOpt = endOpt,
        addressBatchSize = addressBatchSize,
        account = account
      )
    } yield {
      inProgress
    }
  }

  /** Used to call a recursive rescan after the previous rescan is complete. The
    * [[prevState]] parameter is what represents the previous rescan. We wait
    * for this rescan to complete, and then check if we need to do another
    * rescan
    */
  private def recursiveRescan(
      prevState: RescanState.RescanStarted,
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      account: HDAccount
  ): Future[Unit] = {
    val awaitPreviousRescanF =
      RescanState.awaitSingleRescanDone(rescanState = prevState)
    for {
      _ <- awaitPreviousRescanF // this is where the deadlock occurs
      externalGap <- calcAddressGap(HDChainType.External, account)
      changeGap <- calcAddressGap(HDChainType.Change, account)
      _ <- {
        if (
          externalGap >= walletConfig.addressGapLimit && changeGap >= walletConfig.addressGapLimit
        ) {
          logger.info(
            s"Did not find any funds within the last ${walletConfig.addressGapLimit} addresses. Stopping our rescan."
          )
          prevState.recursiveRescanP.success(RescanState.RescanNotNeeded)
          Future.unit
        } else {
          logger.info(
            s"Attempting rescan again with fresh pool of addresses as we had a " +
              s"match within our address gap limit of ${walletConfig.addressGapLimit} externalGap=$externalGap changeGap=$changeGap"
          )
          val recursiveF = doNeutrinoRescan(
            account = account,
            startOpt = startOpt,
            endOpt = endOpt,
            addressBatchSize = addressBatchSize,
            forceGenerateSpks = true
          )
          recursiveF.map(r => prevState.recursiveRescanP.success(r))
        }
      }
    } yield {
      ()
    }
  }

  private def calcAddressGap(
      chainType: HDChainType,
      account: HDAccount
  ): Future[Int] = {
    for {
      addressDbs <- addressDAO.findAllForAccount(account)
      addressGap <-
        addressDbs
          // make sure all addressDb are of the correct chainType
          // and they are sorted according to their index so we can
          // calculate the gap accurately
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
      blocks: Vector[DoubleSha256DigestBE]
  ): Future[Unit] = {
    logger.debug(s"Requesting ${blocks.size} block(s)")
    val subs = blocks.map(
      transactionProcessing.subscribeForBlockProcessingCompletionSignal)
    val downloadF = nodeApi.downloadBlocks(blocks)
    for {
      _ <- downloadF
      _ <- Future.sequence(subs)
    } yield ()
  }

  private def matchBlocks(
      endOpt: Option[BlockStamp],
      startOpt: Option[BlockStamp],
      account: HDAccount,
      addressBatchSize: Int,
      forceGenerateSpks: Boolean
  ): Future[RescanState.RescanStarted] = {
    val rescanStateF = for {
      rescanState <- getMatchingBlocks(
        startOpt = startOpt,
        endOpt = endOpt,
        addressBatchSize = addressBatchSize,
        account = account,
        forceGenerateSpks = forceGenerateSpks
      )
    } yield {
      rescanState
    }

    rescanStateF
  }

  /** Given a range of filter heights, we fetch the filters associated with
    * those heights and emit them downstream
    */
  private val fetchFiltersFlow
      : Flow[Vector[Int], Vector[ChainQueryApi.FilterResponse], NotUsed] = {
    // parallelism as 1 here because `getFiltersBetweenHeights`
    // fetches filters in parallel. We can run into our max open requests
    // allowed by akka if we have parallelism more than 1 here
    Flow[Vector[Int]].mapAsync(1) { case range: Vector[Int] =>
      val startHeight = range.head
      val endHeight = range.last
      logger.debug(
        s"Searching filters from start=$startHeight to end=$endHeight"
      )
      chainQueryApi.getFiltersBetweenHeights(
        startHeight = startHeight,
        endHeight = endHeight
      )
    }
  }

  /** Searches the given block filters against the given scriptPubKeys for
    * matches. If there is a match, request the full block to search
    */
  private def searchFiltersForMatches(
      scripts: Vector[ScriptPubKey],
      filtersResponse: Vector[ChainQueryApi.FilterResponse],
      parallelismLevel: Int
  ): Future[Vector[BlockMatchingResponse]] = {
    val startHeightOpt = filtersResponse.headOption.map(_.blockHeight)
    val endHeightOpt = filtersResponse.lastOption.map(_.blockHeight)
    for {
      filtered <- findMatches(filtersResponse, scripts, parallelismLevel)
      _ <- downloadAndProcessBlocks(filtered.map(_.blockHash))
    } yield {
      logger.debug(
        s"Found ${filtered.length} matches from start=$startHeightOpt to end=$endHeightOpt"
      )
      filtered
    }
  }

  override def findMatches(
      filters: Vector[FilterResponse],
      scripts: Vector[ScriptPubKey],
      parallelismLevel: Int
  ): Future[Vector[BlockMatchingResponse]] = {
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
          FutureUtil.makeAsync { () =>
            // Find any matches in the group and add the corresponding block hashes into the result
            filterGroup
              .foldLeft(Vector.empty[BlockMatchingResponse]) {
                (blocks, filter) =>
                  val matcher = SimpleFilterMatcher(filter.compactFilter)
                  if (matcher.matchesAny(bytes)) {
                    logger.info(s"Found a match in block ${filter.blockHeight}")
                    blocks :+ BlockMatchingResponse(
                      filter.blockHash,
                      filter.blockHeight
                    )
                  } else {
                    blocks
                  }
              }
          }
        })
        .map(_.flatten)
    }
  }

  /** Calculates group size to split a filter vector into [[parallelismLevel]]
    * groups. It's needed to limit number of threads required to run the
    * matching
    */
  private def calcGroupSize(vectorSize: Int, parallelismLevel: Int): Int = {
    if (vectorSize / parallelismLevel * parallelismLevel < vectorSize)
      vectorSize / parallelismLevel + 1
    else vectorSize / parallelismLevel
  }
}
