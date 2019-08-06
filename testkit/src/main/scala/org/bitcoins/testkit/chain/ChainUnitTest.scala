package org.bitcoins.testkit.chain

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  BlockHeaderDb,
  BlockHeaderDbHelper
}
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, ChainParams}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.chain
import org.bitcoins.testkit.chain.fixture._
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.zmq.ZMQSubscriber
import org.scalatest._
import play.api.libs.json.{JsError, JsSuccess, Json}
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.db.AppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig

trait ChainUnitTest
    extends org.scalatest.fixture.AsyncFlatSpec
    with BitcoinSFixture
    with ChainFixtureHelper
    with MustMatchers
    with BitcoinSLogger
    with BeforeAndAfter
    with BeforeAndAfterAll {

  implicit def system: ActorSystem

  val timeout: FiniteDuration = 10.seconds

  implicit lazy val chainParam: ChainParams = appConfig.chain

  implicit lazy val appConfig: ChainAppConfig =
    BitcoinSTestAppConfig.getTestConfig()

  /**
    * Behaves exactly like the default conf, execpt
    * network is set to mainnet
    */
  lazy val mainnetAppConfig: ChainAppConfig = {
    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    BitcoinSTestAppConfig.getTestConfig(mainnetConf)
  }

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(appConfig)
  }

  implicit def ec: ExecutionContext =
    system.dispatcher

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
                destroy = () => ChainUnitTest.destroyHeaderTable)(test)
  }

  def withPopulatedBlockHeaderDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(build = () => ChainUnitTest.createPopulatedBlockHeaderDAO,
                destroy = () => ChainUnitTest.destroyHeaderTable)(test)
  }

  def withChainHandler(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(() => ChainUnitTest.createChainHandler,
                () => ChainUnitTest.destroyHeaderTable)(test)
  }

  /** Creates and populates BlockHeaderTable with block headers 562375 to 571375 */
  def createPopulatedChainHandler(): Future[ChainHandler] = {
    for {
      blockHeaderDAO <- ChainUnitTest.createPopulatedBlockHeaderDAO()
      chainHandler <- ChainHandler.fromDatabase(blockHeaderDAO = blockHeaderDAO)
    } yield chainHandler
  }

  def withPopulatedChainHandler(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(() => createPopulatedChainHandler,
                () => ChainUnitTest.destroyHeaderTable)(test)
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

  def createChainApiWithBitcoindRpc(
      bitcoind: BitcoindRpcClient): Future[BitcoindChainHandlerViaRpc] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()

    val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)

    chainHandlerF.map { handler =>
      chain.fixture.BitcoindChainHandlerViaRpc(bitcoind, handler)
    }

  }

  def createBitcoindChainHandlerViaZmq(): Future[BitcoindChainHandlerViaZmq] = {
    composeBuildersAndWrap(() => BitcoinSFixture.createBitcoind,
                           createChainHandlerWithBitcoindZmq,
                           BitcoindChainHandlerViaZmq.apply)()
  }

  def destroyBitcoindChainHandlerViaZmq(
      bitcoindChainHandler: BitcoindChainHandlerViaZmq): Future[Unit] = {

    //piggy back off of rpc destructor
    val rpc = chain.fixture.BitcoindChainHandlerViaRpc(
      bitcoindChainHandler.bitcoindRpc,
      bitcoindChainHandler.chainHandler)

    destroyBitcoindChainApiViaRpc(rpc).map { _ =>
      bitcoindChainHandler.zmqSubscriber.stop
    }
  }

  def destroyBitcoindChainApiViaRpc(
      bitcoindChainHandler: BitcoindChainHandlerViaRpc): Future[Unit] = {
    val stopBitcoindF =
      BitcoindRpcTestUtil.stopServer(bitcoindChainHandler.bitcoindRpc)
    val dropTableF = ChainUnitTest.destroyHeaderTable()
    stopBitcoindF.flatMap(_ => dropTableF)
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
      composeBuildersAndWrap(builder = () => BitcoinSFixture.createBitcoind,
                             dependentBuilder =
                               createChainHandlerWithBitcoindZmq,
                             wrap = BitcoindChainHandlerViaZmq.apply)

    makeDependentFixture(builder, destroyBitcoindChainHandlerViaZmq)(test)
  }

  def withBitcoindChainHandlerViaRpc(test: OneArgAsyncTest)(
      implicit system: ActorSystem): FutureOutcome = {
    val builder: () => Future[BitcoindChainHandlerViaRpc] = { () =>
      BitcoinSFixture.createBitcoind().flatMap(createChainApiWithBitcoindRpc(_))
    }

    makeDependentFixture(builder, destroyBitcoindChainApiViaRpc)(test)
  }

  override def afterAll(): Unit = {
    system.terminate()
    ()
  }
}

object ChainUnitTest extends BitcoinSLogger {

  /** Height of the first block in populated fixtures */
  val FIRST_BLOCK_HEIGHT: Int = 562375

  val FIRST_POW_CHANGE: Int = (FIRST_BLOCK_HEIGHT / 2016 + 1) * 2016

  val genesisHeaderDb: BlockHeaderDb = ChainTestUtil.regTestGenesisHeaderDb

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

    import org.bitcoins.rpc.serializers.JsonReaders.BlockHeaderReads
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

  def destroyHeaderTable()(implicit appConfig: ChainAppConfig): Future[Unit] = {
    ChainDbManagement.dropHeaderTable()
  }

  def destroyBitcoind(bitcoind: BitcoindRpcClient)(
      implicit system: ActorSystem): Future[Unit] = {
    BitcoindRpcTestUtil.stopServer(bitcoind)
  }

  /** Creates the [[org.bitcoins.chain.models.BlockHeaderTable]] */
  private def setupHeaderTable()(
      implicit appConfig: ChainAppConfig,
      ec: ExecutionContext): Future[Unit] = {
    ChainDbManagement.createHeaderTable(createIfNotExists = true)
  }

  /** Creates the [[org.bitcoins.chain.models.BlockHeaderTable]] and inserts the genesis header */
  def setupHeaderTableWithGenesisHeader()(
      implicit ec: ExecutionContext,
      appConfig: ChainAppConfig): Future[(ChainHandler, BlockHeaderDb)] = {
    val tableSetupF = setupHeaderTable()

    val chainHandlerF = makeChainHandler()

    val genesisHeaderF = tableSetupF.flatMap { _ =>
      for {
        chainHandler <- chainHandlerF
        genHeader <- chainHandler.blockHeaderDAO.create(genesisHeaderDb)
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

    ChainHandler.fromDatabase(blockHeaderDAO = blockHeaderDAO)

  }

}
