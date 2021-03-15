package org.bitcoins.testkit.node

import akka.actor.{ActorSystem, Cancellable}
import org.bitcoins.core.api.chain.{ChainApi, ChainQueryApi, FilterSyncMarker}
import org.bitcoins.core.api.chain.db.{
  BlockHeaderDb,
  CompactFilterDb,
  CompactFilterHeaderDb
}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.gcs.FilterHeader
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.AppConfig
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

/** A base test trait for all the tests in our nodeTest module */
trait BaseNodeTest extends BitcoinSFixture with EmbeddedPg {

  /** Wallet config with data directory set to user temp directory */
  implicit protected def getFreshConfig: BitcoinSAppConfig

  implicit override lazy val np: NetworkParameters =
    getFreshConfig.nodeConf.network

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(getFreshConfig.nodeConf)
    super[EmbeddedPg].beforeAll()
  }

  override def afterAll(): Unit = {
    super[EmbeddedPg].afterAll()
  }

  lazy val junkAddress: BitcoinAddress =
    BitcoinAddress("2NFyxovf6MyxfHqtVjstGzs6HeLqv92Nq4U")

  /** Helper method to generate blocks every interval
    * @return a cancellable that will stop generating blocks
    */
  def genBlockInterval(bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem): Cancellable = {

    var counter = 0
    val desiredBlocks = 5
    val interval = 500.millis

    val genBlock = new Runnable {
      override def run(): Unit = {
        if (counter < desiredBlocks) {
          bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(1, _))
          counter = counter + 1
        }
      }
    }

    system.scheduler.scheduleAtFixedRate(2.second, interval)(genBlock)
  }

  def getBIP39PasswordOpt(): Option[String] =
    KeyManagerTestUtil.bip39PasswordOpt

  val genesisChainApi: ChainApi = new ChainApi {

    override def processHeaders(
        headers: Vector[BlockHeader]): Future[ChainApi] =
      Future.successful(this)

    override def getHeader(
        hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]] =
      Future.successful(None)

    override def getHeadersAtHeight(
        height: Int): Future[Vector[BlockHeaderDb]] =
      Future.successful(Vector.empty)

    override def getBlockCount(): Future[Int] = Future.successful(0)

    override def getBestBlockHeader(): Future[BlockHeaderDb] =
      Future.successful(ChainUnitTest.genesisHeaderDb)

    override def processFilterHeaders(
        filterHeaders: Vector[FilterHeader],
        stopHash: DoubleSha256DigestBE): Future[ChainApi] =
      Future.successful(this)

    override def nextBlockHeaderBatchRange(
        stopHash: DoubleSha256DigestBE,
        batchSize: Int): Future[Option[FilterSyncMarker]] =
      Future.successful(None)

    override def nextFilterHeaderBatchRange(
        startHeight: Int,
        batchSize: Int): Future[Option[FilterSyncMarker]] =
      Future.successful(None)

    override def processFilters(
        message: Vector[CompactFilterMessage]): Future[ChainApi] =
      Future.successful(this)

    override def processCheckpoints(
        checkpoints: Vector[DoubleSha256DigestBE],
        blockHash: DoubleSha256DigestBE): Future[ChainApi] =
      Future.successful(this)

    override def getFilterHeaderCount(): Future[Int] = Future.successful(0)

    override def getFilterHeadersAtHeight(
        height: Int): Future[Vector[CompactFilterHeaderDb]] =
      Future.successful(Vector.empty)

    override def getBestFilterHeader(): Future[Option[CompactFilterHeaderDb]] =
      Future.successful(None)

    override def getFilterHeader(blockHash: DoubleSha256DigestBE): Future[
      Option[CompactFilterHeaderDb]] = Future.successful(None)

    override def getFilter(
        hash: DoubleSha256DigestBE): Future[Option[CompactFilterDb]] =
      Future.successful(None)

    override def getFilterCount(): Future[Int] = Future.successful(0)

    override def getFiltersAtHeight(
        height: Int): Future[Vector[CompactFilterDb]] =
      Future.successful(Vector.empty)

    override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
      Future.successful(0)

    override def getHeadersBetween(
        from: BlockHeaderDb,
        to: BlockHeaderDb): Future[Vector[BlockHeaderDb]] =
      Future.successful(Vector.empty)

    override def getBlockHeight(
        blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
      Future.successful(None)

    override def getBestBlockHash(): Future[DoubleSha256DigestBE] =
      Future.successful(DoubleSha256DigestBE.empty)

    override def getNumberOfConfirmations(
        blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]] =
      Future.successful(None)

    override def getFiltersBetweenHeights(
        startHeight: Int,
        endHeight: Int): Future[Vector[ChainQueryApi.FilterResponse]] =
      Future.successful(Vector.empty)

    override def epochSecondToBlockHeight(time: Long): Future[Int] =
      Future.successful(0)
  }
}
