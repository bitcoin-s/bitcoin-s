package org.bitcoins.testkit.chain

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.{Sink, Source, SourceQueueWithComplete}
import com.typesafe.config.ConfigFactory
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.blockchain.sync.ChainSync
import org.bitcoins.chain.blockchain.{ChainHandler, ChainHandlerCached}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.*
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db.*
import org.bitcoins.core.config.{MainNet, RegTest, SigNet, TestNet3, TestNet4}
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.chain.ChainUnitTest.{
  createChainHandler,
  createChainHandlerCached
}
import org.bitcoins.testkit.chain.fixture.*
import org.bitcoins.testkit.chain.models.{
  ReorgFixtureBlockHeaderDAO,
  ReorgFixtureChainApi
}
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.{BitcoinSTestAppConfig, chain}
import org.bitcoins.testkitcore.chain.ChainTestUtil
import org.bitcoins.zmq.ZMQSubscriber
import org.scalatest.*
import play.api.libs.json.{JsError, JsSuccess, Json}

import java.net.InetSocketAddress
import java.nio.file.Files
import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

trait ChainUnitTest extends BitcoinSFixture {

  def chainAppConfig: ChainAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig().chainConf

  /** Behaves exactly like the default conf, execpt network is set to mainnet
    */
  def mainnetAppConfig: ChainAppConfig = {
    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    BitcoinSTestAppConfig.getNeutrinoTestConfig(mainnetConf).chainConf
  }

  override def afterAll(): Unit = {
    super[BitcoinSFixture].afterAll()
  }

  /** Fixture that creates a block header table with one row inserted into it,
    * the [[org.bitcoins.core.protocol.blockchain.RegTestNetChainParams]]
    * genesis block
    */
  def withBlockHeaderDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      build = () => {
        val c = chainAppConfig
        c.start().flatMap { _ =>
          ChainUnitTest.createBlockHeaderDAO()(using executionContext, c)
        }
      },
      destroy = (blockHeaderDAO: BlockHeaderDAO) => {
        ChainUnitTest.destroyChainApi()(
          using system,
          blockHeaderDAO.appConfig
        )
      }
    )(test)
  }

  def withChainAppConfig(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      build = () => {
        Future.successful(
          BitcoinSTestAppConfig.getNeutrinoTestConfig().chainConf)
      },
      destroy = (chainAppConfig: ChainAppConfig) =>
        ChainUnitTest.destroyChainApi()(using system, chainAppConfig)
    )(test)
  }

  /** Creates a compact filter DAO with zero rows in it */
  def withCompactFilterHeaderDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      build = () => {
        val c = chainAppConfig
        c.start().flatMap { _ =>
          ChainUnitTest.createFilterHeaderDAO()(using c, executionContext)
        }
      },
      destroy = (fhDAO: CompactFilterHeaderDAO) =>
        ChainUnitTest.destroyChainApi()(using system, fhDAO.appConfig)
    )(test)
  }

  /** Creates a compact filter DAO with zero rows in it */
  def withCompactFilterDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      build = () => {
        val c = chainAppConfig
        c.start().flatMap { _ =>
          ChainUnitTest.createFilterDAO()(using c, executionContext)
        }
      },
      destroy = (cfDAO: CompactFilterDAO) =>
        ChainUnitTest.destroyChainApi()(using system, cfDAO.appConfig)
    )(test)
  }

  def withPopulatedBlockHeaderDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      build = () => {
        val c = chainAppConfig
        c.start().flatMap { _ =>
          ChainUnitTest
            .createPopulatedBlockHeaderDAO()(using c, executionContext)
        }
      },
      destroy = (bhDAO: BlockHeaderDAO) =>
        ChainUnitTest.destroyChainApi()(using system, bhDAO.appConfig)
    )(test)
  }

  def withChainStateDescriptorDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      build = () => {
        val c = chainAppConfig
        c.start()
          .flatMap(_ =>
            ChainUnitTest
              .createChainStateDescriptorDAO()(using executionContext, c))

      },
      destroy = (csDAO: ChainStateDescriptorDAO) =>
        ChainUnitTest.destroyChainApi()(using system, csDAO.appConfig)
    )(test)
  }

  def withChainHandler(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      () => {
        val c = chainAppConfig
        c.start()
          .flatMap(_ =>
            ChainUnitTest.createChainHandler()(using executionContext, c))
      },
      (ch: ChainHandler) =>
        ChainUnitTest
          .destroyChainApi()(using system, chainAppConfig = ch.chainConfig)
    )(test)
  }

  def withChainHandlerCached(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      () => {
        val c = chainAppConfig
        c.start().flatMap { _ =>
          ChainUnitTest.createChainHandlerCached()(using executionContext, c)
        }
      },
      (chc: ChainHandler) =>
        ChainUnitTest.destroyChainApi()(using system, chc.chainConfig)
    )(test)
  }

  def withChainHandlerGenesisFilter(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      () => {
        val c = chainAppConfig
        c.start().flatMap(_ => createChainHandlerWithGenesisFilter()(using c))
      },
      (chgf: ChainHandler) =>
        ChainUnitTest.destroyChainApi()(using system, chgf.chainConfig)
    )(test)
  }

  def withChainHandlerCachedGenesisFilter(
      test: OneArgAsyncTest
  ): FutureOutcome = {
    makeDependentFixture(
      build = () => {
        val c = chainAppConfig
        c.start()
          .flatMap(_ => createChainHandlerCachedWithGenesisFilter()(using c))

      },
      destroy = (chcgf: ChainHandler) =>
        ChainUnitTest.destroyChainApi()(using system, chcgf.chainConfig)
    )(test)
  }

  def createChainHandlerWithGenesisFilter()(implicit
      chainAppConfig: ChainAppConfig): Future[ChainHandler] = {
    for {
      chainHandler <- createChainHandler()
      filterHeaderChainApi <- chainHandler.processFilterHeader(
        ChainTestUtil.regTestGenesisHeaderCompactFilterHeaderDb.filterHeader,
        ChainTestUtil.regTestGenesisHeaderDb.hashBE
      )
      filterChainApi <-
        filterHeaderChainApi.processFilter(
          ChainTestUtil.regtestGenesisFilterMessage)
    } yield filterChainApi.asInstanceOf[ChainHandler]
  }

  def createChainHandlerCachedWithGenesisFilter()(implicit
      chainAppConfig: ChainAppConfig): Future[ChainHandlerCached] = {
    for {
      chainHandler <- createChainHandlerCached()
      filterHeaderChainApi <- chainHandler.processFilterHeader(
        ChainTestUtil.regTestGenesisHeaderCompactFilterHeaderDb.filterHeader,
        ChainTestUtil.regTestGenesisHeaderDb.hashBE
      )
      filterChainApi <-
        filterHeaderChainApi.processFilter(
          ChainTestUtil.regtestGenesisFilterMessage)
    } yield filterChainApi.asInstanceOf[ChainHandlerCached]
  }

  def createChainHandlerWithBitcoindZmq(bitcoind: BitcoindRpcClient)(implicit
      chainAppConfig: ChainAppConfig
  ): Future[(ChainHandler, ZMQSubscriber)] = {
    val startedF = chainAppConfig.isStarted().flatMap { isStarted =>
      if (isStarted) {
        Future.unit
      } else {
        chainAppConfig.start()
      }
    }
    startedF.flatMap { _ =>
      val zmqRawBlockUriOpt: Option[InetSocketAddress] =
        bitcoind.instance.zmqConfig.rawBlock
      val sink = Sink.foreachAsync(1) { (blocks: Vector[Block]) =>
        val chainApi =
          ChainHandler.fromDatabase()(using executionContext, chainAppConfig)
        val processF = chainApi.processHeaders(blocks.map(_.blockHeader))
        processF.failed.foreach { err =>
          logger.error(
            s"Failed to parse handleRawBlock callback for block=${blocks.map(_.blockHeader.hashBE)}",
            err)
        }
        processF.map(_ => ())
      }
      val queue: SourceQueueWithComplete[Block] = Source
        .queue[Block](10, OverflowStrategy.backpressure)
        .batch(100, { t => Vector(t) }) { (vec, t) => vec.appended(t) }
        .to(sink)
        .run()(using Materializer.matFromSystem(using system))
      val handleRawBlock: Block => Unit = { (block: Block) =>
        queue
          .offer(block)
          .foreach {
            case org.apache.pekko.stream.QueueOfferResult.Enqueued => // all good
            case org.apache.pekko.stream.QueueOfferResult.Dropped =>
              logger.warn(
                s"Block offer dropped for block ${block.blockHeader.hashBE}")
            case org.apache.pekko.stream.QueueOfferResult.Failure(ex) =>
              logger.error(
                s"Block offer failed for block ${block.blockHeader.hashBE}",
                ex)
            case org.apache.pekko.stream.QueueOfferResult.QueueClosed =>
              logger.error(
                s"Block offer failed: queue closed for block ${block.blockHeader.hashBE}")
          }(using executionContext)
        ()
      }

      val socket = zmqRawBlockUriOpt.get
      val zmqSubscriber =
        new ZMQSubscriber(
          socket = socket,
          hashTxListener = None,
          hashBlockListener = None,
          rawTxListener = None,
          rawBlockListener = Some(handleRawBlock)
        )
      zmqSubscriber.start()

      val handlerWithGenesisHeaderF =
        ChainUnitTest.setupHeaderTableWithGenesisHeader()(
          using executionContext,
          chainAppConfig
        )

      val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)

      // generate a block and make sure we see it so we know the subscription is complete
      val subscribedF = for {
        chainHandler <- chainHandlerF
        _ <- ChainUnitTest.isSynced(chainHandler, bitcoind)
        addr <- bitcoind.getNewAddress
        hash <- bitcoind.generateToAddress(1, addr).map(_.head)
        // wait until we see the hash, to make sure the subscription is started
        _ <- AsyncUtil.retryUntilSatisfiedF(
          () => {
            chainHandler.getHeader(hash).map(_.isDefined)
          },
          1.second
        )
      } yield {
        (ChainHandler.fromDatabase()(using executionContext, chainAppConfig),
         zmqSubscriber)
      }
      subscribedF
    }

  }

  def destroyBitcoindChainHandlerViaZmq(
      bitcoindChainHandler: BitcoindChainHandlerViaZmq
  ): Future[Unit] = {

    // piggy back off of rpc destructor
    val rpc = chain.fixture.BitcoindBaseVersionChainHandlerViaRpc(
      bitcoindChainHandler.bitcoindRpc,
      bitcoindChainHandler.chainHandler
    )

    ChainUnitTest
      .destroyBitcoindChainApiViaRpc(rpc)(
        using system,
        bitcoindChainHandler.chainHandler.chainConfig)
      .map { _ =>
        bitcoindChainHandler.zmqSubscriber.stop()
      }
  }

  /** Creates a
    * [[org.bitcoins.rpc.client.common.BitcoindRpcClient BitcoindRpcClient]]
    * that is linked to our
    * [[org.bitcoins.chain.blockchain.ChainHandler ChainHandler]] via a
    * [[org.bitcoins.zmq.ZMQSubscriber zmq]]. This means messages are passed
    * between bitcoin and our chain handler with a zmq pub/sub message passing
    * @param test
    *   the test to be executed with bitcoind and chain handler via zmq
    * @param system
    * @return
    */
  def withBitcoindChainHandlerViaZmq(test: OneArgAsyncTest)(implicit
      system: ActorSystem,
      chainAppConfig: ChainAppConfig
  ): FutureOutcome = {
    val builder: () => Future[BitcoindChainHandlerViaZmq] =
      BitcoinSFixture.composeBuildersAndWrap(
        builder = () => BitcoinSFixture.createBitcoind(),
        dependentBuilder = { (rpc: BitcoindRpcClient) =>
          createChainHandlerWithBitcoindZmq(rpc)(using chainAppConfig)
        },
        wrap = BitcoindChainHandlerViaZmq.apply
      )

    makeDependentFixture(builder, destroyBitcoindChainHandlerViaZmq)(test)
  }

  final def processHeaders(
      processorF: Future[ChainApi],
      headers: Vector[BlockHeader],
      height: Int
  ): Future[Assertion] = {

    def processedHeadersF = {
      for {
        chainApi <- processorF
        chainApiWithHeaders <-
          FutureUtil.foldLeftAsync(chainApi, headers.grouped(2000).toVector)(
            (chainApi, headers) => chainApi.processHeaders(headers))
        res <- FutureUtil.foldLeftAsync(
          (
            Option.empty[BlockHeaderDb],
            height,
            Vector.empty[Assertion]
          ),
          headers
        ) { case ((prevHeaderDbOpt, height, assertions), header) =>
          for {
            headerOpt <- chainApiWithHeaders.getHeader(header.hashBE)
          } yield {
            val chainWork = prevHeaderDbOpt match {
              case None => Pow.getBlockProof(header)
              case Some(prevHeader) =>
                prevHeader.chainWork + Pow.getBlockProof(header)
            }

            val expectedBlockHeaderDb =
              BlockHeaderDbHelper.fromBlockHeader(height, chainWork, header)

            val newHeight = height + 1

            val newAssertions =
              assertions :+ assert(headerOpt.contains(expectedBlockHeaderDb))

            (Some(expectedBlockHeaderDb), newHeight, newAssertions)
          }
        }
      } yield {
        res
      }
    }
    processedHeadersF.map(_ => succeed)
  }

  /** Builds two competing headers that are built from the same parent */
  private def buildCompetingHeaders(
      parent: BlockHeaderDb
  ): (BlockHeader, BlockHeader) = {
    val newHeaderCandidate1 =
      BlockHeaderHelper.buildNextHeader(parent)

    val newHeaderCandidate2 =
      BlockHeaderHelper.buildNextHeader(parent)
    if (newHeaderCandidate1.chainWork >= newHeaderCandidate2.chainWork) {
      (newHeaderCandidate1.blockHeader, newHeaderCandidate2.blockHeader)
    } else {
      (newHeaderCandidate2.blockHeader, newHeaderCandidate1.blockHeader)
    }
  }

  /** Builds two competing headers off of the
    * [[ChainHandler.getBestBlockHash best chain tip]]
    */
  def buildChainHandlerCompetingHeaders(
      chainHandler: ChainHandler
  ): Future[ReorgFixtureChainApi] = {
    for {
      oldBestTip <- chainHandler.getBestBlockHeader()
      (newHeaderB, newHeaderC) = buildCompetingHeaders(oldBestTip)
      newChainApi <- chainHandler.processHeader(newHeaderB)
      newChainApi2 <- newChainApi.processHeader(newHeaderC)
      newHeaderDbB <- newChainApi.getHeader(newHeaderB.hashBE)
      newHeaderDbC <- newChainApi2.getHeader(newHeaderC.hashBE)
    } yield {
      ReorgFixtureChainApi(
        newChainApi2,
        newHeaderDbB.get,
        newHeaderDbC.get,
        oldBestTip
      )
    }
  }

  /** Builds two competing headers off of [[BlockHeaderDAO.chainTips]]. */
  def buildBlockHeaderDAOCompetingHeaders(
      blockHeaderDAO: BlockHeaderDAO
  ): Future[ReorgFixtureBlockHeaderDAO] = {
    implicit val chainAppConfig: ChainAppConfig = blockHeaderDAO.appConfig
    val handler = ChainHandler.fromDatabase(
      blockHeaderDAO,
      CompactFilterHeaderDAO(),
      CompactFilterDAO(),
      ChainStateDescriptorDAO()
    )
    val chainFixtureF = buildChainHandlerCompetingHeaders(handler)
    for {
      chainFixture <- chainFixtureF
    } yield {
      ReorgFixtureBlockHeaderDAO(
        blockHeaderDAO = blockHeaderDAO,
        headerDb1 = chainFixture.headerDb1,
        headerDb2 = chainFixture.headerDb2,
        oldBestBlockHeader = chainFixture.oldBestBlockHeader
      )
    }
  }
}

object ChainUnitTest extends ChainVerificationLogger {

  /** Height of the first block in populated fixtures */
  val FIRST_BLOCK_HEIGHT: Int = 562375

  val FIRST_POW_CHANGE: Int = (FIRST_BLOCK_HEIGHT / 2016 + 1) * 2016

  def createChainHandler()(implicit
      ec: ExecutionContext,
      appConfig: ChainAppConfig
  ): Future[ChainHandler] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()
    for {
      _ <- handlerWithGenesisHeaderF
      c = ChainHandler.fromDatabase()
    } yield c
  }

  def createChainHandlerCached()(implicit
      ec: ExecutionContext,
      appConfig: ChainAppConfig
  ): Future[ChainHandlerCached] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()

    val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)
    chainHandlerF
  }

  def createBlockHeaderDAO()(implicit
      ec: ExecutionContext,
      appConfig: ChainAppConfig
  ): Future[BlockHeaderDAO] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()

    val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)
    chainHandlerF.map(_.blockHeaderDAO)
  }

  def createChainStateDescriptorDAO()(implicit
      ec: ExecutionContext,
      appConfig: ChainAppConfig
  ): Future[ChainStateDescriptorDAO] = {
    appConfig.migrate()
    Future.successful(ChainStateDescriptorDAO())
  }

  def createFilterHeaderDAO()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext
  ): Future[CompactFilterHeaderDAO] = {
    appConfig.migrate()
    Future.successful(CompactFilterHeaderDAO())
  }

  def createPopulatedFilterHeaderDAO()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext
  ): Future[CompactFilterHeaderDAO] = {
    createFilterHeaderDAO()
  }

  def createFilterDAO()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext
  ): Future[CompactFilterDAO] = {
    appConfig.migrate()
    Future.successful(CompactFilterDAO())
  }

  def createPopulatedFilterDAO()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext
  ): Future[CompactFilterDAO] = {
    createFilterDAO()
  }

  /** Creates and populates BlockHeaderTable with block headers 562375 to 571375
    */
  def createPopulatedBlockHeaderDAO()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext
  ): Future[BlockHeaderDAO] = {
    // The height of the first block in the json file
    val OFFSET: Int = FIRST_BLOCK_HEIGHT

    val source =
      scala.io.Source.fromURL(getClass.getResource("/block_headers.json"))
    val arrStr = source.getLines().next()
    source.close()

    import org.bitcoins.commons.serializers.JsonReaders.BlockHeaderReads
    val headersResult = Json.parse(arrStr).validate[Vector[BlockHeader]]

    headersResult match {
      case err: JsError =>
        logger.error(s"Failed to parse headers from block_headers.json: $err")
        Future.failed(new RuntimeException(err.toString))
      case JsSuccess(headers, _) =>
        var prevHeaderOpt: Option[BlockHeaderDb] = None
        val dbHeaders = headers.zipWithIndex.map { case (header, height) =>
          val chainWork = prevHeaderOpt match {
            case None => Pow.getBlockProof(header)
            case Some(prevHeader) =>
              prevHeader.chainWork + Pow.getBlockProof(header)
          }

          val newHeader = BlockHeaderDbHelper.fromBlockHeader(
            height + OFFSET,
            chainWork,
            header
          )
          prevHeaderOpt = Some(newHeader)
          newHeader
        }

        @tailrec
        def splitIntoBatches(
            batchSize: Int,
            dbHeaders: Vector[BlockHeaderDb],
            batchesSoFar: Vector[Vector[BlockHeaderDb]]
        ): Vector[Vector[BlockHeaderDb]] = {
          if (dbHeaders.isEmpty) {
            batchesSoFar
          } else if (dbHeaders.length < batchSize) {
            batchesSoFar.:+(dbHeaders)
          } else {
            val (batch, nextDbHeaders) = dbHeaders.splitAt(batchSize)
            val nextBatchesSoFar = batchesSoFar.:+(batch)

            splitIntoBatches(batchSize, nextDbHeaders, nextBatchesSoFar)
          }
        }

        val batchedDbHeaders = splitIntoBatches(
          batchSize = 500,
          dbHeaders = dbHeaders,
          batchesSoFar = Vector.empty
        )

        for {
          _ <- ChainUnitTest.setupAllTables()
          chainHandler <- ChainUnitTest.makeChainHandler()
          _ <- batchedDbHeaders.foldLeft(
            Future.successful[Vector[BlockHeaderDb]](Vector.empty)
          ) { case (fut, batch) =>
            for {
              _ <- fut
              headers <- chainHandler.blockHeaderDAO.createAll(batch)
            } yield headers
          }
        } yield {
          chainHandler.blockHeaderDAO
        }
    }
  }

  def createChainApiWithBitcoindRpc(bitcoind: BitcoindRpcClient)(implicit
      ec: ExecutionContext,
      chainAppConfig: ChainAppConfig
  ): Future[BitcoindBaseVersionChainHandlerViaRpc] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()

    val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)

    chainHandlerF.map { handler =>
      chain.fixture.BitcoindBaseVersionChainHandlerViaRpc(bitcoind, handler)
    }
  }

  def destroyBitcoindChainApiViaRpc(
      bitcoindChainHandler: BitcoindBaseVersionChainHandlerViaRpc
  )(implicit
      system: ActorSystem,
      chainAppConfig: ChainAppConfig
  ): Future[Unit] = {
    import system.dispatcher
    val stopBitcoindF =
      BitcoindRpcTestUtil.stopServer(bitcoindChainHandler.bitcoindRpc)
    val dropTableF = ChainUnitTest.destroyAllTables()
    val stoppedChainAppConfigF = dropTableF.flatMap(_ => chainAppConfig.stop())
    for {
      _ <- stopBitcoindF
      _ <- stoppedChainAppConfigF
    } yield ()
  }

  def destroyBitcoind(
      bitcoind: BitcoindRpcClient
  )(implicit system: ActorSystem): Future[Unit] = {
    BitcoindRpcTestUtil.stopServer(bitcoind)
  }

  def setupAllTables()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext
  ): Future[Unit] =
    Future {
      appConfig.migrate()
      ()
    }

  def destroyAllTables()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext
  ): Future[Unit] =
    for {
      _ <- appConfig.dropTable("flyway_schema_history")
      _ <- appConfig.dropAll()
    } yield ()

  def setupHeaderTableWithGenesisHeader()(implicit
      ec: ExecutionContext,
      appConfig: ChainAppConfig
  ): Future[(ChainHandlerCached, BlockHeaderDb)] = {
    val tableSetupF = setupAllTables()

    val genesisHeaderF = tableSetupF.flatMap { _ =>
      val chainHandlerF = makeChainHandler()
      for {
        chainHandler <- chainHandlerF
        header = appConfig.network match {
          case MainNet  => ChainTestUtil.mainnetGenesisHeaderDb
          case TestNet3 => ChainTestUtil.testnet3GenesisHeaderDb
          case TestNet4 => ChainTestUtil.testnet4GenesisHeaderDb
          case RegTest  => ChainTestUtil.regTestGenesisHeaderDb
          case SigNet   => ChainTestUtil.signetGenesisHeaderDb
        }
        genHeader <- chainHandler.blockHeaderDAO.upsert(
          header
        )
      } yield genHeader
    }

    for {
      genHeader <- genesisHeaderF
      chainHandler <- makeChainHandler()
    } yield (chainHandler, genHeader)
  }

  def makeChainHandler()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext
  ): Future[ChainHandlerCached] = {
    lazy val blockHeaderDAO = BlockHeaderDAO()
    lazy val filterHeaderDAO = CompactFilterHeaderDAO()
    lazy val filterDAO = CompactFilterDAO()
    lazy val stateDAO = ChainStateDescriptorDAO()

    ChainHandlerCached.fromDatabase(
      blockHeaderDAO = blockHeaderDAO,
      filterHeaderDAO = filterHeaderDAO,
      filterDAO = filterDAO,
      stateDAO = stateDAO
    )

  }

  /** Syncs the given chain handler to the given bitcoind */
  def syncFromBitcoind(bitcoind: BitcoindRpcClient, chainHandler: ChainHandler)(
      implicit ec: ExecutionContext
  ): Future[ChainApi] = {
    // sync headers
    // first we need to implement the 'getBestBlockHashFunc' and 'getBlockHeaderFunc' functions
    val getBestBlockHashFunc = { () =>
      bitcoind.getBestBlockHash()
    }

    val getBlockHeaderFunc = { (hash: DoubleSha256DigestBE) =>
      bitcoind.getBlockHeader(hash).map(_.blockHeader)
    }

    ChainSync.sync(chainHandler, getBlockHeaderFunc, getBestBlockHashFunc)
  }

  /** Destroys the chain api, but leaves the bitcoind instance running so we can
    * cache it
    */
  def destroyChainApi()(implicit
      system: ActorSystem,
      chainAppConfig: ChainAppConfig
  ): Future[Unit] = {
    import system.dispatcher
    if (Files.exists(chainAppConfig.datadir)) {
      // check if we even created the database
      // in some test cases - such as ChainAppConfigTest -
      // we don't create the database for the test case
      for {
        _ <- ChainUnitTest.destroyAllTables()
        _ <- chainAppConfig.stop()
      } yield ()
    } else {
      Future.unit
    }

  }

  def buildNHeaders(chainHandler: ChainHandler, target: Int)(implicit
      ec: ExecutionContext,
      mat: Materializer
  ): Future[Unit] = {
    val bestHeaderF = chainHandler.getBestBlockHeader()
    val builtHeaderDbsF: Future[Vector[BlockHeaderDb]] = bestHeaderF
      .flatMap { bestHeader =>
        val builder = Vector.newBuilder[BlockHeaderDb]
        val doneF = Source(0.until(target))
          .fold(bestHeader) { case (bestHeader, _) =>
            val nextHeader = BlockHeaderHelper.buildNextHeader(bestHeader)
            builder.addOne(nextHeader)
            nextHeader
          }
          .runWith(Sink.ignore)

        doneF.map(_ => builder.result())
      }

    for {
      builtHeaderDbs <- builtHeaderDbsF
      _ <- chainHandler.processHeaders(builtHeaderDbs.map(_.blockHeader))
    } yield ()
  }

  def isSynced(chainApi: ChainApi, bitcoind: BitcoindRpcClient)(implicit
      ec: ExecutionContext): Future[Boolean] = {
    for {
      bitcoinSBestBlockHash <- chainApi.getBestBlockHash()
      bitcoindBestBlockHash <- bitcoind.getBestBlockHash()
    } yield bitcoinSBestBlockHash == bitcoindBestBlockHash
  }
}
