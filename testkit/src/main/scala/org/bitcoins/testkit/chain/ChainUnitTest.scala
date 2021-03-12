package org.bitcoins.testkit.chain

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.blockchain.{ChainHandler, ChainHandlerCached}
import org.bitcoins.chain.blockchain.sync.ChainSync
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models._
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db._
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.AppConfig
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.testkit.chain.ChainUnitTest.createChainHandler
import org.bitcoins.testkit.chain.fixture._
import org.bitcoins.testkit.chain.models.{
  ReorgFixtureBlockHeaderDAO,
  ReorgFixtureChainApi
}
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.node.CachedChainAppConfig
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.{AkkaUtil, ScalaTestUtil}
import org.bitcoins.testkit.{chain, BitcoinSTestAppConfig}
import org.bitcoins.zmq.ZMQSubscriber
import org.scalatest._
import play.api.libs.json.{JsError, JsSuccess, Json}

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.DurationInt

trait ChainUnitTest
    extends BitcoinSFixture
    with ChainFixtureHelper
    with CachedChainAppConfig {

  /** Behaves exactly like the default conf, execpt
    * network is set to mainnet
    */
  lazy val mainnetAppConfig: ChainAppConfig = {
    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    BitcoinSTestAppConfig.getSpvTestConfig(mainnetConf)
  }

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(cachedChainConf)
  }

  override def afterAll(): Unit = {
    super[CachedChainAppConfig].afterAll()
    super[BitcoinSFixture].afterAll()
  }

  /** All untagged tests will be given this tag. Override this if you are using
    * ChainFixture and the plurality of tests use some fixture other than Empty.
    */
  val defaultTag: ChainFixtureTag = ChainFixtureTag.Empty

  def withChainFixture(test: OneArgAsyncTest): FutureOutcome = {
    val stringTag =
      test.tags.headOption.getOrElse(ChainFixtureTag.defaultTag.name)

    val fixtureTag: ChainFixtureTag = ChainFixtureTag.from(stringTag)

    makeDependentFixture(build = () => createFixture(fixtureTag),
                         destroy = destroyFixture)(test)
  }

  /** This is a wrapper for a tagged test statement that adds a def inFixtured
    * to replace the use of in, which only accepts a FixtureParam => Future[Assertion],
    * whereas inFixtured accepts a PartialFunction and fails the test if it is not
    * defined on the input.
    *
    * This is nothing more than syntactic sugar.
    *
    * This functionality is added using language.implicitConversions below
    */
  final class SugaryItVerbStringTaggedAs(
      itVerbStringTaggedAs: ItVerbStringTaggedAs) {

    def inFixtured(
        partialTestFun: PartialFunction[
          FixtureParam,
          Future[compatible.Assertion]])(implicit
        pos: org.scalactic.source.Position): Unit = {
      val testFun: FixtureParam => Future[compatible.Assertion] = {
        fixture: FixtureParam =>
          partialTestFun.applyOrElse[FixtureParam, Future[Assertion]](
            fixture,
            { _: FixtureParam =>
              Future(fail("Incorrect tag/fixture for this test"))
            })
      }

      itVerbStringTaggedAs.in(testFun)(pos)
    }
  }

  /** This is a wrapper for a tagged test statement that adds a def inFixtured
    * to replace the use of in, which only accepts a FixtureParam => Future[Assertion],
    * whereas inFixtured accepts a PartialFunction and fails the test if it is not
    * defined on the input.
    *
    * This is nothing more than syntactic sugar.
    *
    * This functionality is added using language.implicitConversions below
    */
  final class SugaryItVerbString(itVerbString: ItVerbString) {

    def inFixtured(
        partialTestFun: PartialFunction[
          FixtureParam,
          Future[compatible.Assertion]])(implicit
        pos: org.scalactic.source.Position): Unit = {
      val testFun: FixtureParam => Future[compatible.Assertion] = {
        fixture: FixtureParam =>
          partialTestFun.applyOrElse[FixtureParam, Future[Assertion]](
            fixture,
            { _: FixtureParam =>
              Future(fail("Incorrect tag/fixture for this test"))
            })
      }

      itVerbString.in(testFun)(pos)
    }
  }

  import language.implicitConversions

  implicit def itVerbStringTaggedAsToSugaryItVerbStringTaggedAs(
      itVerbStringTaggedAs: ItVerbStringTaggedAs): SugaryItVerbStringTaggedAs =
    new SugaryItVerbStringTaggedAs(itVerbStringTaggedAs)

  implicit def itVerbStringToSugaryItVerbString(
      itVerbString: ItVerbString): SugaryItVerbString =
    new SugaryItVerbString(itVerbString)

  /** Fixture that creates a [[org.bitcoins.chain.models.BlockHeaderTable]]
    * with one row inserted into it, the [[org.bitcoins.core.protocol.blockchain.RegTestNetChainParams]]
    * genesis block
    */
  def withBlockHeaderDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(build = () => ChainUnitTest.createBlockHeaderDAO(),
                destroy = () => ChainUnitTest.destroyAllTables())(test)
  }

  /** Creates a compact filter DAO with zero rows in it */
  def withCompactFilterHeaderDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(build = () => ChainUnitTest.createFilterHeaderDAO(),
                destroy = () => ChainUnitTest.destroyAllTables())(test)
  }

  /** Creates a compact filter DAO with zero rows in it */
  def withCompactFilterDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(build = () => ChainUnitTest.createFilterDAO(),
                destroy = () => ChainUnitTest.destroyAllTables())(test)
  }

  def withPopulatedBlockHeaderDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(build = () => ChainUnitTest.createPopulatedBlockHeaderDAO(),
                destroy = () => ChainUnitTest.destroyAllTables())(test)
  }

  def withChainHandler(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(() => ChainUnitTest.createChainHandler(),
                () => ChainUnitTest.destroyAllTables())(test)
  }

  def withChainHandlerCached(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(() => ChainUnitTest.createChainHandlerCached(),
                () => ChainUnitTest.destroyAllTables())(test)
  }

  def withChainHandlerGenesisFilter(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(() => createChainHandlerWithGenesisFilter(),
                () => ChainUnitTest.destroyAllTables())(test)
  }

  def withChainHandlerCachedGenesisFilter(
      test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(build = () => createChainHandlerCachedWithGenesisFilter(),
                destroy = () => ChainUnitTest.destroyAllTables())(test)
  }

  /** Creates and populates BlockHeaderTable with block headers 562375 to 571375 */
  def createPopulatedChainHandler(): Future[ChainHandler] = {
    for {
      blockHeaderDAO <- ChainUnitTest.createPopulatedBlockHeaderDAO()
      filterHeaderDAO <- ChainUnitTest.createPopulatedFilterHeaderDAO()
      filterDAO <- ChainUnitTest.createPopulatedFilterDAO()
      chainHandler = ChainHandler.fromDatabase(blockHeaderDAO = blockHeaderDAO,
                                               filterHeaderDAO =
                                                 filterHeaderDAO,
                                               filterDAO = filterDAO)
    } yield chainHandler
  }

  def createChainHandlerWithGenesisFilter(): Future[ChainHandler] = {
    for {
      chainHandler <- createChainHandler()
      filterHeaderChainApi <- chainHandler.processFilterHeader(
        ChainUnitTest.genesisFilterHeaderDb.filterHeader,
        ChainUnitTest.genesisHeaderDb.hashBE)
      filterChainApi <-
        filterHeaderChainApi.processFilter(ChainUnitTest.genesisFilterMessage)
    } yield filterChainApi.asInstanceOf[ChainHandler]
  }

  def createChainHandlerCachedWithGenesisFilter(): Future[
    ChainHandlerCached] = {
    for {
      chainHandler <- createChainHandler()
      filterHeaderChainApi <- chainHandler.processFilterHeader(
        ChainUnitTest.genesisFilterHeaderDb.filterHeader,
        ChainUnitTest.genesisHeaderDb.hashBE)
      filterChainApi <-
        filterHeaderChainApi.processFilter(ChainUnitTest.genesisFilterMessage)
    } yield filterChainApi.asInstanceOf[ChainHandlerCached]
  }

  def withPopulatedChainHandler(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(() => createPopulatedChainHandler(),
                () => ChainUnitTest.destroyAllTables())(test)
  }

  def createChainHandlerWithBitcoindZmq(
      bitcoind: BitcoindRpcClient): Future[(ChainHandler, ZMQSubscriber)] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()

    val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)

    val zmqRawBlockUriOpt: Option[InetSocketAddress] =
      bitcoind.instance.zmqConfig.rawBlock

    val handleRawBlock: Block => Unit = { block: Block =>
      chainHandlerF.flatMap(_.processHeader(block.blockHeader))
      ()
    }

    val socket = zmqRawBlockUriOpt.get
    val zmqSubscriber =
      new ZMQSubscriber(socket = socket,
                        hashTxListener = None,
                        hashBlockListener = None,
                        rawTxListener = None,
                        rawBlockListener = Some(handleRawBlock))
    zmqSubscriber.start()

    for {
      _ <- AkkaUtil.nonBlockingSleep(1.second)
      chainHandler <- chainHandlerF
    } yield (chainHandler, zmqSubscriber)
  }

  def createBitcoindChainHandlerViaZmq(): Future[BitcoindChainHandlerViaZmq] = {
    composeBuildersAndWrap(() => BitcoinSFixture.createBitcoind(),
                           createChainHandlerWithBitcoindZmq,
                           BitcoindChainHandlerViaZmq.apply)()
  }

  def destroyBitcoindChainHandlerViaZmq(
      bitcoindChainHandler: BitcoindChainHandlerViaZmq): Future[Unit] = {

    //piggy back off of rpc destructor
    val rpc = chain.fixture.BitcoindChainHandlerViaRpc(
      bitcoindChainHandler.bitcoindRpc,
      bitcoindChainHandler.chainHandler)

    ChainUnitTest.destroyBitcoindChainApiViaRpc(rpc).map { _ =>
      bitcoindChainHandler.zmqSubscriber.stop()
    }
  }

  /** Creates a [[org.bitcoins.rpc.client.common.BitcoindRpcClient BitcoindRpcClient]] that is linked to our [[org.bitcoins.chain.blockchain.ChainHandler ChainHandler]]
    * via a [[org.bitcoins.zmq.ZMQSubscriber zmq]]. This means messages are passed between bitcoin and our chain handler
    * with a zmq pub/sub message passing
    * @param test the test to be executed with bitcoind and chain handler via zmq
    * @param system
    * @return
    */
  def withBitcoindChainHandlerViaZmq(test: OneArgAsyncTest)(implicit
      system: ActorSystem): FutureOutcome = {
    val builder: () => Future[BitcoindChainHandlerViaZmq] =
      composeBuildersAndWrap(builder = () => BitcoinSFixture.createBitcoind(),
                             dependentBuilder =
                               createChainHandlerWithBitcoindZmq,
                             wrap = BitcoindChainHandlerViaZmq.apply)

    makeDependentFixture(builder, destroyBitcoindChainHandlerViaZmq)(test)
  }

  def withBitcoindChainHandlerViaRpc(test: OneArgAsyncTest)(implicit
      system: ActorSystem): FutureOutcome = {
    val builder: () => Future[BitcoindChainHandlerViaRpc] = { () =>
      BitcoinSFixture
        .createBitcoind()
        .flatMap(ChainUnitTest.createChainApiWithBitcoindRpc)
    }

    makeDependentFixture(builder, ChainUnitTest.destroyBitcoindChainApiViaRpc)(
      test)
  }

  def withBitcoindV19ChainHandlerViaRpc(test: OneArgAsyncTest)(implicit
      system: ActorSystem): FutureOutcome = {
    val builder: () => Future[BitcoindV19ChainHandler] = { () =>
      ChainUnitTest.createBitcoindV19ChainHandler()
    }
    makeDependentFixture(builder, ChainUnitTest.destroyBitcoindV19ChainApi)(
      test)
  }

  final def processHeaders(
      processorF: Future[ChainApi],
      headers: Vector[BlockHeader],
      height: Int): Future[Assertion] = {

    def processedHeadersF =
      for {
        chainApi <- processorF
        chainApiWithHeaders <-
          FutureUtil.foldLeftAsync(chainApi, headers.grouped(2000).toVector)(
            (chainApi, headers) => chainApi.processHeaders(headers))
      } yield {
        FutureUtil.foldLeftAsync((Option.empty[BlockHeaderDb],
                                  height,
                                  Vector.empty[Future[Assertion]]),
                                 headers) {
          case ((prevHeaderDbOpt, height, assertions), header) =>
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

              val newAssertions = assertions :+ Future(
                assert(headerOpt.contains(expectedBlockHeaderDb)))

              (Some(expectedBlockHeaderDb), newHeight, newAssertions)
            }
        }
      }

    for {
      processedHeaders <- processedHeadersF
      (_, _, vecFutAssert) <- processedHeaders
      assertion <- ScalaTestUtil.toAssertF(vecFutAssert)
    } yield {
      assertion
    }

  }

  /** Builds two competing headers that are built from the same parent */
  private def buildCompetingHeaders(
      parent: BlockHeaderDb): (BlockHeader, BlockHeader) = {
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

  /** Builds two competing headers off of the [[ChainHandler.getBestBlockHash best chain tip]] */
  def buildChainHandlerCompetingHeaders(
      chainHandler: ChainHandler): Future[ReorgFixtureChainApi] = {
    for {
      oldBestTip <- chainHandler.getBestBlockHeader()
      (newHeaderB, newHeaderC) = buildCompetingHeaders(oldBestTip)
      newChainApi <- chainHandler.processHeader(newHeaderB)
      newChainApi2 <- newChainApi.processHeader(newHeaderC)
      newHeaderDbB <- newChainApi.getHeader(newHeaderB.hashBE)
      newHeaderDbC <- newChainApi2.getHeader(newHeaderC.hashBE)
    } yield {
      ReorgFixtureChainApi(newChainApi2,
                           newHeaderDbB.get,
                           newHeaderDbC.get,
                           oldBestTip)
    }
  }

  /** Builds two competing headers off of [[BlockHeaderDAO.chainTips]]. */
  def buildBlockHeaderDAOCompetingHeaders(
      blockHeaderDAO: BlockHeaderDAO): Future[ReorgFixtureBlockHeaderDAO] = {
    val handler = ChainHandler.fromDatabase(blockHeaderDAO,
                                            CompactFilterHeaderDAO(),
                                            CompactFilterDAO())
    val chainFixtureF = buildChainHandlerCompetingHeaders(handler)
    for {
      chainFixture <- chainFixtureF
    } yield {
      ReorgFixtureBlockHeaderDAO(blockHeaderDAO = blockHeaderDAO,
                                 headerDb1 = chainFixture.headerDb1,
                                 headerDb2 = chainFixture.headerDb2,
                                 oldBestBlockHeader =
                                   chainFixture.oldBestBlockHeader)
    }
  }
}

object ChainUnitTest extends ChainVerificationLogger {

  /** Height of the first block in populated fixtures */
  val FIRST_BLOCK_HEIGHT: Int = 562375

  val FIRST_POW_CHANGE: Int = (FIRST_BLOCK_HEIGHT / 2016 + 1) * 2016

  val genesisHeaderDb: BlockHeaderDb = ChainTestUtil.regTestGenesisHeaderDb

  val genesisFilterDb: CompactFilterDb =
    ChainTestUtil.regTestGenesisHeaderCompactFilterDb

  val genesisFilterHeaderDb: CompactFilterHeaderDb =
    ChainTestUtil.regTestGenesisHeaderCompactFilterHeaderDb

  val genesisFilterMessage: CompactFilterMessage = {
    CompactFilterMessage(filterType = genesisFilterDb.filterType,
                         blockHash = genesisFilterDb.blockHashBE.flip,
                         filterBytes = genesisFilterDb.golombFilter.bytes)
  }

  def createChainHandler()(implicit
      ec: ExecutionContext,
      appConfig: ChainAppConfig): Future[ChainHandler] = {
    createChainHandlerCached()
  }

  def createChainHandlerCached()(implicit
      ec: ExecutionContext,
      appConfig: ChainAppConfig): Future[ChainHandlerCached] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()

    val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)
    chainHandlerF
  }

  def createBlockHeaderDAO()(implicit
      ec: ExecutionContext,
      appConfig: ChainAppConfig): Future[BlockHeaderDAO] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()

    val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)
    chainHandlerF.map(_.blockHeaderDAO)
  }

  def createFilterHeaderDAO()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[CompactFilterHeaderDAO] = {
    appConfig.migrate()
    Future.successful(CompactFilterHeaderDAO())
  }

  def createPopulatedFilterHeaderDAO()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[CompactFilterHeaderDAO] = {
    createFilterHeaderDAO()
  }

  def createFilterDAO()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[CompactFilterDAO] = {
    appConfig.migrate()
    Future.successful(CompactFilterDAO())
  }

  def createPopulatedFilterDAO()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[CompactFilterDAO] = {
    createFilterDAO()
  }

  /** Creates and populates BlockHeaderTable with block headers 562375 to 571375 */
  def createPopulatedBlockHeaderDAO()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[BlockHeaderDAO] = {
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

          val newHeader = BlockHeaderDbHelper.fromBlockHeader(height + OFFSET,
                                                              chainWork,
                                                              header)
          prevHeaderOpt = Some(newHeader)
          newHeader
        }

        @tailrec
        def splitIntoBatches(
            batchSize: Int,
            dbHeaders: Vector[BlockHeaderDb],
            batchesSoFar: Vector[Vector[BlockHeaderDb]]): Vector[
          Vector[BlockHeaderDb]] = {
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

        val batchedDbHeaders = splitIntoBatches(batchSize = 500,
                                                dbHeaders = dbHeaders,
                                                batchesSoFar = Vector.empty)

        for {
          _ <- ChainUnitTest.setupAllTables()
          chainHandler <- ChainUnitTest.makeChainHandler()
          _ <- batchedDbHeaders.foldLeft(
            Future.successful[Vector[BlockHeaderDb]](Vector.empty)) {
            case (fut, batch) =>
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
      chainAppConfig: ChainAppConfig): Future[BitcoindChainHandlerViaRpc] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()

    val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)

    chainHandlerF.map { handler =>
      chain.fixture.BitcoindChainHandlerViaRpc(bitcoind, handler)
    }
  }

  def destroyBitcoindChainApiViaRpc(
      bitcoindChainHandler: BitcoindChainHandlerViaRpc)(implicit
      system: ActorSystem,
      chainAppConfig: ChainAppConfig): Future[Unit] = {
    import system.dispatcher
    val stopBitcoindF =
      BitcoindRpcTestUtil.stopServer(bitcoindChainHandler.bitcoindRpc)
    val dropTableF = ChainUnitTest.destroyAllTables()
    stopBitcoindF.flatMap(_ => dropTableF)
  }

  def createBitcoindV19ChainHandler()(implicit
      system: ActorSystem,
      chainAppConfig: ChainAppConfig): Future[BitcoindV19ChainHandler] = {
    import system.dispatcher
    val bitcoindV = BitcoindVersion.V19
    BitcoinSFixture
      .createBitcoind(Some(bitcoindV))
      .flatMap(createChainApiWithBitcoindRpc)
      .map { b: BitcoindChainHandlerViaRpc =>
        BitcoindV19ChainHandler(
          b.bitcoindRpc.asInstanceOf[BitcoindV19RpcClient],
          b.chainHandler)
      }
  }

  def destroyBitcoindV19ChainApi(
      bitcoindV19ChainHandler: BitcoindV19ChainHandler)(implicit
      system: ActorSystem,
      chainAppConfig: ChainAppConfig): Future[Unit] = {
    val b = BitcoindChainHandlerViaRpc(bitcoindV19ChainHandler.bitcoind,
                                       bitcoindV19ChainHandler.chainHandler)
    destroyBitcoindChainApiViaRpc(b)
  }

  def destroyBitcoind(bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem): Future[Unit] = {
    BitcoindRpcTestUtil.stopServer(bitcoind)
  }

  def setupAllTables()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[Unit] =
    Future {
      appConfig.migrate()
      ()
    }

  def destroyAllTables()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[Unit] =
    for {
      _ <- appConfig.dropTable("flyway_schema_history")
      _ <- appConfig.dropAll()
    } yield ()

  /** Creates the [[org.bitcoins.chain.models.BlockHeaderTable]] and inserts the genesis header */
  def setupHeaderTableWithGenesisHeader()(implicit
      ec: ExecutionContext,
      appConfig: ChainAppConfig): Future[
    (ChainHandlerCached, BlockHeaderDb)] = {
    val tableSetupF = setupAllTables()

    val genesisHeaderF = tableSetupF.flatMap { _ =>
      val chainHandlerF = makeChainHandler()
      for {
        chainHandler <- chainHandlerF
        genHeader <- chainHandler.blockHeaderDAO.upsert(genesisHeaderDb)
      } yield genHeader
    }

    for {
      genHeader <- genesisHeaderF
      chainHandler <- makeChainHandler()
    } yield (chainHandler, genHeader)
  }

  def makeChainHandler()(implicit
      appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[ChainHandlerCached] = {
    lazy val blockHeaderDAO = BlockHeaderDAO()
    lazy val filterHeaderDAO = CompactFilterHeaderDAO()
    lazy val filterDAO = CompactFilterDAO()

    ChainHandlerCached.fromDatabase(blockHeaderDAO = blockHeaderDAO,
                                    filterHeaderDAO = filterHeaderDAO,
                                    filterDAO = filterDAO)

  }

  /** Syncs the given chain handler to the given bitcoind */
  def syncFromBitcoind(bitcoind: BitcoindRpcClient, chainHandler: ChainHandler)(
      implicit ec: ExecutionContext): Future[ChainApi] = {
    //sync headers
    //first we need to implement the 'getBestBlockHashFunc' and 'getBlockHeaderFunc' functions
    val getBestBlockHashFunc = { () =>
      bitcoind.getBestBlockHash
    }

    val getBlockHeaderFunc = { hash: DoubleSha256DigestBE =>
      bitcoind.getBlockHeader(hash).map(_.blockHeader)
    }

    ChainSync.sync(chainHandler, getBlockHeaderFunc, getBestBlockHashFunc)
  }
}
