package org.bitcoins.chain.util

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
  ChainParams,
  RegTestNetChainParams
}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.{DbConfig, UnitTestDbConfig}
import org.bitcoins.testkit.chain.ChainTestUtil
import org.scalatest._
import play.api.libs.json.{JsError, JsSuccess, Json}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future, Promise}

trait ChainUnitTest
    extends fixture.AsyncFlatSpec
    with MustMatchers
    with BitcoinSLogger
    with BeforeAndAfter
    with BeforeAndAfterAll {

  val timeout = 10.seconds
  def dbConfig: DbConfig = UnitTestDbConfig

  val genesisHeaderDb: BlockHeaderDb = ChainTestUtil.regTestGenesisHeaderDb
  val chainParam: ChainParams = RegTestNetChainParams

  lazy val blockHeaderDAO = BlockHeaderDAO(chainParams =
                                             ChainTestUtil.regTestChainParams,
                                           dbConfig = dbConfig)

  lazy val blockchain =
    Blockchain.fromHeaders(Vector(genesisHeaderDb), blockHeaderDAO)

  lazy val chainHandler: ChainHandler = ChainHandler(blockchain)
  lazy val chainApi: ChainApi = chainHandler

  implicit def ec: ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global

  /** Fixture that creates a [[org.bitcoins.chain.models.BlockHeaderTable]]
    * with one row inserted into it, the [[RegTestNetChainParams]]
    * genesis block
    * @param test
    * @return
    */
  def withBlockHeaderDAO(test: OneArgAsyncTest): FutureOutcome = {

    val genesisHeaderF = setupHeaderTableWithGenesisHeader()

    val blockHeaderDAOF = genesisHeaderF.map(_ => blockHeaderDAO)
    val testExecutionF = blockHeaderDAOF.flatMap(dao =>
      test(dao.asInstanceOf[FixtureParam]).toFuture)

    dropHeaderTable(testExecutionF)
  }

  def withPopulatedBlockHeaderDAO(
      test: BlockHeaderDAO => Future[Assertion]): Future[Assertion] = {
    val tableSetupF = setupHeaderTable()

    val source = scala.io.Source.fromFile("block_headers.json")
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
            BlockHeaderDbHelper.fromBlockHeader(height, header)
        }

        val instertedF = tableSetupF.flatMap(_ =>
          chainHandler.blockchain.blockHeaderDAO.createAll(dbHeaders))

        val testExecutionF = instertedF.flatMap(_ => test(blockHeaderDAO))

        dropHeaderTable(testExecutionF)
    }
  }

  def withChainHandler(test: OneArgAsyncTest): FutureOutcome = {

    val genesisHeaderF = setupHeaderTableWithGenesisHeader()
    val chainHandlerF = genesisHeaderF.map(_ => chainHandler)

    val testExecutionF = chainHandlerF.flatMap(handler =>
      test(handler.asInstanceOf[FixtureParam]).toFuture)

    dropHeaderTable(testExecutionF)
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

  /** Drops the header table and returns the given Future[Assertion] after the table is dropped */
  private def dropHeaderTable(
      testExecutionF: Future[Outcome]): FutureOutcome = {
    val dropTableP = Promise[Unit]()
    testExecutionF.onComplete { _ =>
      ChainDbManagement.dropHeaderTable(dbConfig).foreach { _ =>
        dropTableP.success(())
      }
    }

    val outcomeF = dropTableP.future.flatMap(_ => testExecutionF)

    new FutureOutcome(outcomeF)
  }
}
