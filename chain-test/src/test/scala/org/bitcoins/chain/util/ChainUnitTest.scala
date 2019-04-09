package org.bitcoins.chain.util

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.{Blockchain, ChainHandler}
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}
import org.bitcoins.core.protocol.blockchain.{
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
import scodec.bits.ByteVector

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future, Promise}

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

    val genesisHeaderF = setupHeaderTableWithGenesisHeader()

    val blockHeaderDAOF = genesisHeaderF.map(_ => blockHeaderDAO)
    val testExecutionF = blockHeaderDAOF.flatMap(test(_))

    dropHeaderTable(testExecutionF)
  }

  def withChainHandler(
      test: ChainHandler => Future[Assertion]): Future[Assertion] = {

    val genesisHeaderF = setupHeaderTableWithGenesisHeader()
    val chainHandlerF = genesisHeaderF.map(_ => chainHandler)

    val testExecutionF = chainHandlerF.flatMap(test(_))

    dropHeaderTable(testExecutionF)
  }

  /** Creates the [[org.bitcoins.chain.models.BlockHeaderTable]] and inserts the genesis header */
  private def setupHeaderTableWithGenesisHeader(): Future[BlockHeaderDb] = {
    val tableSetupF = ChainDbManagement.createHeaderTable(dbConfig = dbConfig,
                                                          createIfNotExists =
                                                            true)

    val genesisHeaderF = tableSetupF.flatMap(_ =>
      chainHandler.blockchain.blockHeaderDAO.create(genesisHeaderDb))
    genesisHeaderF
  }

  /** Drops the header table and returns the given Future[Assertion] after the table is dropped */
  private def dropHeaderTable(
      testExecutionF: Future[Assertion]): Future[Assertion] = {
    val dropTableP = Promise[Unit]()
    testExecutionF.onComplete { _ =>
      ChainDbManagement.dropHeaderTable(dbConfig).foreach { _ =>
        dropTableP.success(())
      }
    }

    dropTableP.future.flatMap(_ => testExecutionF)
  }

  /** Represents a bitcoind instance paired with a chain handler via zmq */
  case class BitcoindChainHandler(
      bitcoindRpc: BitcoindRpcClient,
      chainHandler: ChainHandler)

  def bitcoindZmqChainHandler(test: BitcoindChainHandler => Future[Assertion])(
      implicit system: ActorSystem): Future[Assertion] = {
    val instance = BitcoindRpcTestUtil.instance()
    val bitcoindF = {
      val bitcoind = new BitcoindRpcClient(instance)
      bitcoind.start().map(_ => bitcoind)
    }
    val zmqRawBlockUriF = bitcoindF.map(_ => instance.zmqConfig.rawBlock)
    val f: ChainHandler => Future[Assertion] = { chainHandler: ChainHandler =>
      val handleRawBlock: ByteVector => Unit = { bytes: ByteVector =>
        val block = Block.fromBytes(bytes)
        chainHandler.processHeader(block.blockHeader)

        ()
      }

      val zmqSubscriberF = zmqRawBlockUriF.map { uriOpt =>
        val socket = uriOpt.get
        val z =
          new ZMQSubscriber(socket, None, None, None, Some(handleRawBlock))
        z.start()
        Thread.sleep(1000)
        z
      }

      val bitcoindChainHandlerF = for {
        _ <- zmqSubscriberF
        bitcoind <- bitcoindF
      } yield BitcoindChainHandler(bitcoind, chainHandler)

      val testExecutionF = bitcoindChainHandlerF
        .flatMap(test(_))

      testExecutionF.onComplete { _ =>
        //kill bitcoind
        bitcoindChainHandlerF.flatMap { bch =>
          BitcoindRpcTestUtil.stopServer(bch.bitcoindRpc)
        }

        //stop zmq
        zmqSubscriberF.map(_.stop)
      }

      testExecutionF
    }

    withChainHandler(f)

  }
}
