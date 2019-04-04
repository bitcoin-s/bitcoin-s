package org.bitcoins.chain.util

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.{Blockchain, ChainHandler}
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  RegTestNetChainParams
}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.{DbConfig, UnitTestDbConfig}
import org.bitcoins.testkit.chain.ChainTestUtil
import org.scalatest._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

trait ChainUnitTest
    extends AsyncFlatSpec
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
  def withBlockHeaderDAO(
      test: BlockHeaderDAO => Future[Assertion]): Future[Assertion] = {
    val tableSetupF = ChainDbManagement.createHeaderTable(dbConfig = dbConfig,
                                                          createIfNotExists =
                                                            true)

    val genesisHeaderF = tableSetupF.flatMap { _ =>
      blockHeaderDAO.create(genesisHeaderDb)
    }

    val blockHeaderDAOF = genesisHeaderF.map(_ => blockHeaderDAO)
    val testExecutionF = blockHeaderDAOF.flatMap(test(_))

    //this isn't async safe, the completion of `testExecutionF`
    //isn't dependent on successful completion of dropHeaderTable
    testExecutionF.onComplete(_ => ChainDbManagement.dropHeaderTable(dbConfig))

    testExecutionF
  }

  def withChainHandler(
      test: ChainHandler => Future[Assertion]): Future[Assertion] = {

    val tableSetupF = ChainDbManagement.createHeaderTable(dbConfig = dbConfig,
                                                          createIfNotExists =
                                                            true)

    val genesisHeaderF = tableSetupF.flatMap(_ =>
      chainHandler.blockchain.blockHeaderDAO.create(genesisHeaderDb))

    val chainHandlerF = genesisHeaderF.map(_ => chainHandler)

    val testExecutionF = chainHandlerF.flatMap(test(_))

    //this isn't async safe, the completion of `testExecutionF`
    //isn't dependent on successful completion of dropHeaderTable
    testExecutionF.onComplete(_ => ChainDbManagement.dropHeaderTable(dbConfig))

    testExecutionF

  }
}
