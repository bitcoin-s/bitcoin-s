package org.bitcoins.chain.util

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import org.bitcoins.chain.blockchain.{Blockchain, ChainHandler}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.db.{ChainDbConfig, ChainDbManagement}
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, ChainParams, RegTestNetChainParams}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.NetworkDb
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.chain.ChainTestUtil
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.zmq.ZMQSubscriber
import org.scalatest._
import play.api.libs.json.{JsError, JsSuccess, Json}
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}

trait ChainUnitTest
    extends fixture.AsyncFlatSpec
    with BitcoinSFixture
    with MustMatchers
    with BitcoinSLogger
    with BeforeAndAfter
    with BeforeAndAfterAll {

  implicit def system: ActorSystem

  val timeout: FiniteDuration = 10.seconds

  def networkDb: NetworkDb = NetworkDb.UnitTestDbConfig
  def dbConfig: ChainDbConfig = ChainDbConfig.UnitTestDbConfig(networkDb)

  val genesisHeaderDb: BlockHeaderDb = ChainTestUtil.regTestGenesisHeaderDb
  val chainParam: ChainParams = networkDb.chain

  lazy val appConfig = ChainAppConfig(dbConfig)
  private lazy val blockHeaderDAO = BlockHeaderDAO(appConfig)

  lazy val blockchain: Blockchain =
    Blockchain.fromHeaders(Vector(genesisHeaderDb), blockHeaderDAO)

  private lazy val chainHandler: ChainHandler = ChainHandler(blockchain)

  implicit def ec: ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global

  /**
    * If a test file uses ChainFixture as its FixtureParam, then
    * using these tags will determine which fixture the test will get.
    *
    * Simply add taggedAs FixtureTag._ to your test before calling inFixtured.
    */
  sealed abstract class FixtureTag(name: String) extends Tag(name)

  object FixtureTag {
    case object Empty extends FixtureTag("Empty")

    case object GenisisBlockHeaderDAO
        extends FixtureTag("GenisisBlockHeaderDAO")

    case object PopulatedBlockHeaderDAO
        extends FixtureTag("PopulatedBlockHeaderDAO")

    case object GenisisChainHandler extends FixtureTag("GenisisChainHandler")

    case object BitcoindZmqChainHandlerWithBlock
        extends FixtureTag("BitcoindZmqChainHandlerWithBlock")

    def from(tag: String): FixtureTag = {
      tag match {
        case Empty.name                   => Empty
        case GenisisBlockHeaderDAO.name   => GenisisBlockHeaderDAO
        case PopulatedBlockHeaderDAO.name => PopulatedBlockHeaderDAO
        case GenisisChainHandler.name     => GenisisChainHandler
        case BitcoindZmqChainHandlerWithBlock.name =>
          BitcoindZmqChainHandlerWithBlock
        case _: String =>
          throw new IllegalArgumentException(s"$tag is not a valid tag")
      }
    }
  }

  /**
    * All untagged tests will be given this tag. Override this if you are using
    * ChainFixture and the plurality of tests use some fixture other than Empty.
    */
  val defaultTag: FixtureTag = FixtureTag.Empty

  /**
    * This ADT represents all Chain test fixtures. If you set this type to be your
    * FixtureParam and override withFixture to be withChainFixutre, then simply tag
    * tests to specify which fixture that test should receive and then use inFixutred
    * which takes a PartialFunction[ChainFixture, Future[Assertion] ] (i.e. just
    * specify the relevant case for your expected fixture)
    */
  sealed trait ChainFixture

  object ChainFixture {
    case object Empty extends ChainFixture

    case class GenisisBlockHeaderDAO(dao: BlockHeaderDAO) extends ChainFixture

    case class PopulatedBlockHeaderDAO(dao: BlockHeaderDAO) extends ChainFixture

    case class GenisisChainHandler(chainHandler: ChainHandler)
        extends ChainFixture

    case class BitcoindZmqChainHandlerWithBlock(
        bitcoindChainHandler: BitcoindChainHandler)
        extends ChainFixture

    def create(tag: FixtureTag): Future[ChainFixture] = {
      tag match {
        case FixtureTag.Empty => Future.successful(ChainFixture.Empty)
        case FixtureTag.GenisisBlockHeaderDAO =>
          createBlockHeaderDAO().map(GenisisBlockHeaderDAO.apply)
        case FixtureTag.PopulatedBlockHeaderDAO =>
          createPopulatedBlockHeaderDAO().map(PopulatedBlockHeaderDAO.apply)
        case FixtureTag.GenisisChainHandler =>
          createChainHandler().map(GenisisChainHandler.apply)
        case FixtureTag.BitcoindZmqChainHandlerWithBlock =>
          createBitcoindChainHandler().map(
            BitcoindZmqChainHandlerWithBlock.apply)
      }
    }

    def destroy(fixture: ChainFixture): Future[Any] = {
      fixture match {
        case Empty                      => Future.successful(())
        case GenisisBlockHeaderDAO(_)   => destroyHeaderTable()
        case PopulatedBlockHeaderDAO(_) => destroyHeaderTable()
        case GenisisChainHandler(_)     => destroyHeaderTable()
        case BitcoindZmqChainHandlerWithBlock(bitcoindHandler) =>
          destroyBitcoindChainHandler(bitcoindHandler)
      }
    }
  }

  def withChainFixture(test: OneArgAsyncTest): FutureOutcome = {
    val stringTag = test.tags.headOption.getOrElse(defaultTag.name)

    val fixtureTag: FixtureTag = FixtureTag.from(stringTag)

    val fixtureF: Future[ChainFixture] = ChainFixture.create(fixtureTag)

    val outcomeF = fixtureF.flatMap(fixture =>
      test(fixture.asInstanceOf[FixtureParam]).toFuture)

    val fixtureTakeDownF = outcomeF.flatMap { outcome =>
      val destroyedF =
        fixtureF.flatMap(fixture => ChainFixture.destroy(fixture))

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
              Future.successful(fail("Incorrect tag/fixture for this test"))
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
              Future.successful(fail("Incorrect tag/fixture for this test"))
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

  def createBlockHeaderDAO(): Future[BlockHeaderDAO] = {
    val genesisHeaderF = setupHeaderTableWithGenesisHeader()

    genesisHeaderF.map(_ => blockHeaderDAO)
  }

  def destroyHeaderTable(): Future[Unit] = {
    ChainDbManagement.dropHeaderTable(dbConfig)
  }

  /**
    * Fixture that creates a [[org.bitcoins.chain.models.BlockHeaderTable]]
    * with one row inserted into it, the [[RegTestNetChainParams]]
    * genesis block
    */
  def withBlockHeaderDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(createBlockHeaderDAO, destroyHeaderTable)(test)
  }

  // Creates and populates BlockHeaderTable with block headers 562375 to 571375
  def createPopulatedBlockHeaderDAO(): Future[BlockHeaderDAO] = {
    // The height of the first block in the json file
    val OFFSET: Int = 562375

    val tableSetupF = setupHeaderTable()

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

        val insertedF = tableSetupF.flatMap { _ =>
          batchedDbHeaders.foldLeft(
            Future.successful[Vector[BlockHeaderDb]](Vector.empty)) {
            case (fut, batch) =>
              fut.flatMap(_ =>
                chainHandler.blockchain.blockHeaderDAO.createAll(batch))
          }
        }

        insertedF.map(_ => blockHeaderDAO)
    }
  }

  def withPopulatedBlockHeaderDAO(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(createPopulatedBlockHeaderDAO, destroyHeaderTable)(test)
  }

  def createChainHandler(): Future[ChainHandler] = {
    val genesisHeaderF = setupHeaderTableWithGenesisHeader()
    genesisHeaderF.map(_ => chainHandler)
  }

  def withChainHandler(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(createChainHandler, destroyHeaderTable)(test)
  }

  /** Creates the [[org.bitcoins.chain.models.BlockHeaderTable]] */
  private def setupHeaderTable(): Future[Unit] = {
    ChainDbManagement.createHeaderTable(dbConfig = dbConfig,
                                        createIfNotExists = true)
  }

  /** Creates the [[org.bitcoins.chain.models.BlockHeaderTable]] and inserts the genesis header */
  private def setupHeaderTableWithGenesisHeader(): Future[BlockHeaderDb] = {
    val tableSetupF = setupHeaderTable()

    val genesisHeaderF = tableSetupF.flatMap(_ =>
      chainHandler.blockchain.blockHeaderDAO.create(genesisHeaderDb))
    genesisHeaderF
  }

  /** Represents a bitcoind instance paired with a chain handler via zmq */
  case class BitcoindChainHandler(
      bitcoindRpc: BitcoindRpcClient,
      chainHandler: ChainHandler,
      zmqSubscriber: ZMQSubscriber)

  object BitcoindChainHandler {

    def apply(
        bitcoindRpc: BitcoindRpcClient,
        pair: (ChainHandler, ZMQSubscriber)): BitcoindChainHandler = {
      val (chainHandler, zmqSubscriber) = pair

      BitcoindChainHandler(bitcoindRpc, chainHandler, zmqSubscriber)
    }
  }

  def createChainHandlerWithBitcoindZmq(
      bitcoind: BitcoindRpcClient): Future[(ChainHandler, ZMQSubscriber)] = {
    val genesisHeaderF = setupHeaderTableWithGenesisHeader()

    val zmqRawBlockUriOpt: Option[InetSocketAddress] =
      bitcoind.instance.zmqConfig.rawBlock

    val handleRawBlock: ByteVector => Unit = { bytes: ByteVector =>
      val block = Block.fromBytes(bytes)
      chainHandler.processHeader(block.blockHeader)

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

    genesisHeaderF.map(_ => (chainHandler, zmqSubscriber))
  }

  def createBitcoindChainHandler(): Future[BitcoindChainHandler] = {
    composeBuildersAndWrap(createBitcoind,
                           createChainHandlerWithBitcoindZmq,
                           BitcoindChainHandler.apply)()
  }

  def destroyBitcoindChainHandler(
      bitcoindChainHandler: BitcoindChainHandler): Future[Unit] = {
    val stopBitcoindF =
      BitcoindRpcTestUtil.stopServer(bitcoindChainHandler.bitcoindRpc)
    val dropTableF = destroyHeaderTable()

    bitcoindChainHandler.zmqSubscriber.stop

    stopBitcoindF.flatMap(_ => dropTableF)
  }

  //BitcoindChainHandler => Future[Assertion]
  def withBitcoindZmqChainHandler(test: OneArgAsyncTest)(
      implicit system: ActorSystem): FutureOutcome = {
    val builder: () => Future[BitcoindChainHandler] = composeBuildersAndWrap(
      createBitcoind,
      createChainHandlerWithBitcoindZmq,
      BitcoindChainHandler.apply)

    makeDependentFixture(builder, destroyBitcoindChainHandler)(test)
  }

  override def afterAll(): Unit = {
    system.terminate()
  }
}
