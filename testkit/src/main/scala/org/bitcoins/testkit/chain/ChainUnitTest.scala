package org.bitcoins.testkit.chain

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.blockchain.sync.ChainSync
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models._
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.db.AppConfig
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.testkit.chain.fixture._
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.{chain, BitcoinSTestAppConfig}
import org.bitcoins.zmq.ZMQSubscriber
import org.scalatest._
import play.api.libs.json.{JsError, JsSuccess, Json}
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

trait ChainUnitTest
    extends BitcoinSFixture
    with ChainFixtureHelper
    with ChainVerificationLogger {

  implicit lazy val appConfig: ChainAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig()

  /**
    * Behaves exactly like the default conf, execpt
    * network is set to mainnet
    */
  lazy val mainnetAppConfig: ChainAppConfig = {
    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    BitcoinSTestAppConfig.getSpvTestConfig(mainnetConf)
  }

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(appConfig)
  }

  /**
    * All untagged tests will be given this tag. Override this if you are using
    * ChainFixture and the plurality of tests use some fixture other than Empty.
    */
  val defaultTag: ChainFixtureTag = ChainFixtureTag.Empty

  def withChainFixture(test: OneArgAsyncTest): FutureOutcome = {
    val stringTag =
      test.tags.headOption.getOrElse(ChainFixtureTag.defaultTag.name)

    val fixtureTag: ChainFixtureTag = ChainFixtureTag.from(stringTag)

    val fixtureF: Future[ChainFixture] = createFixture(fixtureTag)

    val outcomeF = fixtureF.flatMap(fixture =>
      test(fixture.asInstanceOf[FixtureParam]).toFuture)

    val fixtureTakeDownF = outcomeF.flatMap { outcome =>
      val destroyedF =
        fixtureF.flatMap(fixture => destroyFixture(fixture))

      destroyedF.map(_ => outcome)
    }

    new FutureOutcome(fixtureTakeDownF)
  }

  /**
    * This is a wrapper for a tagged test statement that adds a def inFixtured
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
          Future[compatible.Assertion]])(
        implicit pos: org.scalactic.source.Position): Unit = {
      val testFun: FixtureParam => Future[compatible.Assertion] = {
        fixture: FixtureParam =>
          partialTestFun.applyOrElse[FixtureParam, Future[Assertion]](fixture, {
            _: FixtureParam =>
              Future(fail("Incorrect tag/fixture for this test"))
          })
      }

      itVerbStringTaggedAs.in(testFun)(pos)
    }
  }

  /**
    * This is a wrapper for a tagged test statement that adds a def inFixtured
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
          Future[compatible.Assertion]])(
        implicit pos: org.scalactic.source.Position): Unit = {
      val testFun: FixtureParam => Future[compatible.Assertion] = {
        fixture: FixtureParam =>
          partialTestFun.applyOrElse[FixtureParam, Future[Assertion]](fixture, {
            _: FixtureParam =>
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

  /**
    * Fixture that creates a [[org.bitcoins.chain.models.BlockHeaderTable]]
    * with one row inserted into it, the [[org.bitcoins.core.protocol.blockchain.RegTestNetChainParams]]
    * genesis block
    */
  def withBlockHeaderDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(build = () => ChainUnitTest.createBlockHeaderDAO,
                destroy = () => ChainUnitTest.destroyAllTables)(test)
  }

  def withPopulatedBlockHeaderDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(build = () => ChainUnitTest.createPopulatedBlockHeaderDAO,
                destroy = () => ChainUnitTest.destroyAllTables)(test)
  }

  def withChainHandler(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(() => ChainUnitTest.createChainHandler,
                () => ChainUnitTest.destroyAllTables)(test)
  }

  /** Creates and populates BlockHeaderTable with block headers 562375 to 571375 */
  def createPopulatedChainHandler(): Future[ChainHandler] = {
    for {
      blockHeaderDAO <- ChainUnitTest.createPopulatedBlockHeaderDAO()
      filterHeaderDAO <- ChainUnitTest.createPopulatedFilterHeaderDAO()
      filterDAO <- ChainUnitTest.createPopulatedFilterDAO()
      chainHandler <- ChainHandler.fromDatabase(blockHeaderDAO = blockHeaderDAO,
                                                filterHeaderDAO =
                                                  filterHeaderDAO,
                                                filterDAO = filterDAO)
    } yield chainHandler
  }

  def withPopulatedChainHandler(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(() => createPopulatedChainHandler,
                () => ChainUnitTest.destroyAllTables)(test)
  }

  def createChainHandlerWithBitcoindZmq(
      bitcoind: BitcoindRpcClient): Future[(ChainHandler, ZMQSubscriber)] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()

    val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)

    val zmqRawBlockUriOpt: Option[InetSocketAddress] =
      bitcoind.instance.zmqConfig.rawBlock

    val handleRawBlock: ByteVector => Unit = { bytes: ByteVector =>
      val block = Block.fromBytes(bytes)
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
    Thread.sleep(1000)

    for {
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
      bitcoindChainHandler.zmqSubscriber.stop
    }
  }

  /**
    * Creates a [[org.bitcoins.rpc.client.common.BitcoindRpcClient BitcoindRpcClient]] that is linked to our [[org.bitcoins.chain.blockchain.ChainHandler ChainHandler]]
    * via a [[org.bitcoins.zmq.ZMQSubscriber zmq]]. This means messages are passed between bitcoin and our chain handler
    * with a zmq pub/sub message passing
    * @param test the test to be executed with bitcoind and chain handler via zmq
    * @param system
    * @return
    */
  def withBitcoindChainHandlerViaZmq(test: OneArgAsyncTest)(
      implicit system: ActorSystem): FutureOutcome = {
    val builder: () => Future[BitcoindChainHandlerViaZmq] =
      composeBuildersAndWrap(builder = () => BitcoinSFixture.createBitcoind(),
                             dependentBuilder =
                               createChainHandlerWithBitcoindZmq,
                             wrap = BitcoindChainHandlerViaZmq.apply)

    makeDependentFixture(builder, destroyBitcoindChainHandlerViaZmq)(test)
  }

  def withBitcoindChainHandlerViaRpc(test: OneArgAsyncTest)(
      implicit system: ActorSystem): FutureOutcome = {
    val builder: () => Future[BitcoindChainHandlerViaRpc] = { () =>
      BitcoinSFixture
        .createBitcoind()
        .flatMap(ChainUnitTest.createChainApiWithBitcoindRpc)
    }

    makeDependentFixture(builder, ChainUnitTest.destroyBitcoindChainApiViaRpc)(
      test)
  }

  def withBitcoindV19ChainHandlerViaRpc(test: OneArgAsyncTest)(
      implicit system: ActorSystem): FutureOutcome = {
    val builder: () => Future[BitcoindV19ChainHandler] = { () =>
      ChainUnitTest.createBitcoindV19ChainHandler()
    }
    makeDependentFixture(builder, ChainUnitTest.destroyBitcoindV19ChainApi)(
      test)
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

  def createChainHandler()(
      implicit ec: ExecutionContext,
      appConfig: ChainAppConfig): Future[ChainHandler] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()

    val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)
    chainHandlerF
  }

  def createBlockHeaderDAO()(
      implicit ec: ExecutionContext,
      appConfig: ChainAppConfig): Future[BlockHeaderDAO] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()

    val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)
    chainHandlerF.map(_.blockHeaderDAO)
  }

  def createFilterHeaderDAO()(
      implicit appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[CompactFilterHeaderDAO] = {
    Future.successful(CompactFilterHeaderDAO())
  }

  def createPopulatedFilterHeaderDAO()(
      implicit appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[CompactFilterHeaderDAO] = {
    createFilterHeaderDAO()
  }

  def createFilterDAO()(
      implicit appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[CompactFilterDAO] = {
    Future.successful(CompactFilterDAO())
  }

  def createPopulatedFilterDAO()(
      implicit appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[CompactFilterDAO] = {
    createFilterDAO()
  }

  /** Creates and populates BlockHeaderTable with block headers 562375 to 571375 */
  def createPopulatedBlockHeaderDAO()(
      implicit appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[BlockHeaderDAO] = {
    // The height of the first block in the json file
    val OFFSET: Int = FIRST_BLOCK_HEIGHT

    val tableSetupF = ChainUnitTest.setupHeaderTable()

    val source =
      scala.io.Source.fromURL(getClass.getResource("/block_headers.json"))
    val arrStr = source.getLines.next
    source.close()

    import org.bitcoins.commons.serializers.JsonReaders.BlockHeaderReads
    val headersResult = Json.parse(arrStr).validate[Vector[BlockHeader]]

    headersResult match {
      case err: JsError =>
        logger.error(s"Failed to parse headers from block_headers.json: $err")
        Future.failed(new RuntimeException(err.toString))
      case JsSuccess(headers, _) =>
        val dbHeaders = headers.zipWithIndex.map {
          case (header, height) =>
            BlockHeaderDbHelper.fromBlockHeader(height + OFFSET, header)
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

        val chainHandlerF = ChainUnitTest.makeChainHandler()

        val insertedF = tableSetupF.flatMap { _ =>
          batchedDbHeaders.foldLeft(
            Future.successful[Vector[BlockHeaderDb]](Vector.empty)) {
            case (fut, batch) =>
              for {
                _ <- fut
                chainHandler <- chainHandlerF
                headers <- chainHandler.blockHeaderDAO.createAll(batch)
              } yield headers
          }
        }

        insertedF.flatMap(_ => chainHandlerF.map(_.blockHeaderDAO))
    }
  }

  def createChainApiWithBitcoindRpc(bitcoind: BitcoindRpcClient)(
      implicit ec: ExecutionContext,
      chainAppConfig: ChainAppConfig): Future[BitcoindChainHandlerViaRpc] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()

    val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)

    chainHandlerF.map { handler =>
      chain.fixture.BitcoindChainHandlerViaRpc(bitcoind, handler)
    }
  }

  def destroyBitcoindChainApiViaRpc(
      bitcoindChainHandler: BitcoindChainHandlerViaRpc)(
      implicit system: ActorSystem,
      chainAppConfig: ChainAppConfig): Future[Unit] = {
    import system.dispatcher
    val stopBitcoindF =
      BitcoindRpcTestUtil.stopServer(bitcoindChainHandler.bitcoindRpc)
    val dropTableF = ChainUnitTest.destroyAllTables()
    stopBitcoindF.flatMap(_ => dropTableF)
  }

  def createBitcoindV19ChainHandler()(
      implicit system: ActorSystem,
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
      bitcoindV19ChainHandler: BitcoindV19ChainHandler)(
      implicit system: ActorSystem,
      chainAppConfig: ChainAppConfig): Future[Unit] = {
    val b = BitcoindChainHandlerViaRpc(bitcoindV19ChainHandler.bitcoind,
                                       bitcoindV19ChainHandler.chainHandler)
    destroyBitcoindChainApiViaRpc(b)
  }

  def destroyBitcoind(bitcoind: BitcoindRpcClient)(
      implicit system: ActorSystem): Future[Unit] = {
    BitcoindRpcTestUtil.stopServer(bitcoind)
  }

  /** Creates the [[org.bitcoins.chain.models.BlockHeaderTable]] */
  private def setupHeaderTable()(
      implicit appConfig: ChainAppConfig): Future[Unit] = {
    appConfig.createHeaderTable(createIfNotExists = true)
  }

  def setupAllTables()(
      implicit appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[Unit] = {
    appConfig.createAll()
  }

  def destroyAllTables()(
      implicit appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[Unit] = {
    appConfig.dropAll()
  }

  /** Creates the [[org.bitcoins.chain.models.BlockHeaderTable]] and inserts the genesis header */
  def setupHeaderTableWithGenesisHeader()(
      implicit ec: ExecutionContext,
      appConfig: ChainAppConfig): Future[(ChainHandler, BlockHeaderDb)] = {
    val tableSetupF = setupAllTables()

    val genesisHeaderF = tableSetupF.flatMap { _ =>
      val chainHandlerF = makeChainHandler()
      for {
        chainHandler <- chainHandlerF
        genHeader <- chainHandler.blockHeaderDAO.create(genesisHeaderDb)
        _ <- chainHandler.filterHeaderDAO.create(genesisFilterHeaderDb)
        _ <- chainHandler.filterDAO.create(genesisFilterDb)
      } yield genHeader
    }

    for {
      genHeader <- genesisHeaderF
      chainHandler <- makeChainHandler()
    } yield (chainHandler, genHeader)
  }

  def makeChainHandler()(
      implicit appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[ChainHandler] = {
    lazy val blockHeaderDAO = BlockHeaderDAO()
    lazy val filterHeaderDAO = CompactFilterHeaderDAO()
    lazy val filterDAO = CompactFilterDAO()

    ChainHandler.fromDatabase(blockHeaderDAO = blockHeaderDAO,
                              filterHeaderDAO = filterHeaderDAO,
                              filterDAO = filterDAO)

  }

  /** Syncs the given chain handler to the given bitcoind */
  def syncFromBitcoind(bitcoind: BitcoindRpcClient, chainHandler: ChainHandler)(
      implicit ec: ExecutionContext,
      chainAppConfig: ChainAppConfig): Future[ChainApi] = {
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
