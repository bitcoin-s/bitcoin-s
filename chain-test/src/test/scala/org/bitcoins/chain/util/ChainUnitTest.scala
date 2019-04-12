package org.bitcoins.chain.util

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.{Blockchain, ChainHandler}
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  BlockHeaderDb,
  BlockHeaderDbHelper
}
import org.bitcoins.core.protocol.blockchain.{
  BlockHeader,
  Block,
  ChainParams,
  RegTestNetChainParams
}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.{DbConfig, UnitTestDbConfig}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.chain.ChainTestUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.zmq.ZMQSubscriber
import org.scalatest._
import play.api.libs.json.{JsError, JsSuccess, Json}
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

trait ChainUnitTest
    extends fixture.AsyncFlatSpec
    with MustMatchers
    with BitcoinSLogger
    with BeforeAndAfter
    with BeforeAndAfterAll {

  implicit def system: ActorSystem

  val timeout = 10.seconds
  def dbConfig: DbConfig = UnitTestDbConfig

  val genesisHeaderDb: BlockHeaderDb = ChainTestUtil.regTestGenesisHeaderDb
  val chainParam: ChainParams = RegTestNetChainParams

  private lazy val blockHeaderDAO = BlockHeaderDAO(
    chainParams = ChainTestUtil.regTestChainParams,
    dbConfig = dbConfig)

  lazy val blockchain =
    Blockchain.fromHeaders(Vector(genesisHeaderDb), blockHeaderDAO)

  private lazy val chainHandler: ChainHandler = ChainHandler(blockchain)

  implicit def ec: ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global

  /**
    * Given functions to build and destroy a fixture, returns a OneArgAsyncTest => FutureOutcome
    * (this version gives the destroy function access to the fixture)
    *
    * Example:
    * {{{
    *   makeDependentFixture(createBitcoindChainHandler, destroyBitcoindChainHandler)
    * }}}
    */
  def makeDependentFixture[T](
      build: () => Future[T],
      destroy: T => Future[Any])(test: OneArgAsyncTest): FutureOutcome = {
    val fixtureF = build()

    val outcomeF = fixtureF.flatMap { fixture =>
      test(fixture.asInstanceOf[FixtureParam]).toFuture
    }

    val destroyP = Promise[Unit]()
    outcomeF.onComplete { _ =>
      fixtureF.foreach { fixture =>
        destroy(fixture).onComplete {
          case Success(_)   => destroyP.success(())
          case Failure(err) => destroyP.failure(err)
        }
      }
    }

    val outcomeAfterDestroyF = destroyP.future.flatMap(_ => outcomeF)

    new FutureOutcome(outcomeAfterDestroyF)
  }

  /**
    * Given functions to build and destroy a fixture, returns a OneArgAsyncTest => FutureOutcome
    * (this version does not give the destroy function access to the fixture, see makeDependentFixture)
    *
    * Example:
    * {{{
    *   makeFixture(createBlockHeaderDAO, destroyBlockHeaderTable)
    * }}}
    */
  def makeFixture[T](build: () => Future[T], destroy: () => Future[Any])(
      test: OneArgAsyncTest): FutureOutcome = {
    val outcomeF = build().flatMap { fixture =>
      test(fixture.asInstanceOf[FixtureParam]).toFuture
    }

    val destroyP = Promise[Unit]()
    outcomeF.onComplete { _ =>
      destroy().onComplete {
        case Success(_)   => destroyP.success(())
        case Failure(err) => destroyP.failure(err)
      }
    }

    val outcomeAfterDestroyF = destroyP.future.flatMap(_ => outcomeF)

    new FutureOutcome(outcomeAfterDestroyF)
  }

  /**
    * Given two fixture building methods (one dependent on the other), returns a single
    * fixture building method where the fixture is the pair of the two.
    *
    * Example:
    * {{{
    *   composeBuilders(createBlockHeaderDAO, createChainHandlerFromBlockHeaderDAO)
    * }}}
    */
  def composeBuilders[T, U](
      builder: () => Future[T],
      dependentBuilder: T => Future[U]): () => Future[(T, U)] = () => {
    builder().flatMap { first =>
      dependentBuilder(first).map { second =>
        (first, second)
      }
    }
  }

  /**
    * Given two fixture building methods (one dependent on the other) and a wrapper
    * for their pair type, returns a single fixture building method where the fixture is wrapper.
    *
    * Example:
    * {{{
    *   composeBuildersAndWrap(
    *       createBitcoind,
    *       createChainHandlerWithBitcoindZmq,
    *       BitcoindChainHandler.apply)
    * }}}
    */
  def composeBuildersAndWrap[T, U, C](
      builder: () => Future[T],
      dependentBuilder: T => Future[U],
      wrap: (T, U) => C): () => Future[C] = () => {
    composeBuilders(builder, dependentBuilder)().map {
      case (first, second) => wrap(first, second)
    }
  }

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

  def createBitcoind()(
      implicit system: ActorSystem): Future[BitcoindRpcClient] = {
    val instance = BitcoindRpcTestUtil.instance()
    val bitcoind = new BitcoindRpcClient(instance)

    bitcoind.start().map(_ => bitcoind)
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
