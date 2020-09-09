package org.bitcoins.chain.blockchain

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.api.chain.db.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.testkit.chain.{
  ChainDbUnitTest,
  ChainTestUtil,
  ChainUnitTest
}
import org.bitcoins.testkit.chain.fixture.ChainFixtureTag
import org.bitcoins.testkit.util.FileUtil
import org.scalatest.FutureOutcome
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.io.BufferedSource

class MainnetChainHandlerTest extends ChainDbUnitTest {

  override type FixtureParam = ChainHandler

  override val defaultTag: ChainFixtureTag = ChainFixtureTag.GenisisChainHandler

  implicit override lazy val appConfig: ChainAppConfig = mainnetAppConfig

  val source: BufferedSource = FileUtil.getFileAsSource("block_headers.json")
  val arrStr: String = source.getLines.next
  source.close()

  import org.bitcoins.commons.serializers.JsonReaders.BlockHeaderReads

  val headersResult: Vector[BlockHeader] =
    Json.parse(arrStr).validate[Vector[BlockHeader]].get

  val genesis: BlockHeaderDb = ChainUnitTest.genesisHeaderDb

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainHandler(test)

  behavior of "MainnetChainHandler"

  it must "benchmark ChainHandler.processHeaders()" in {
    chainHandler: ChainHandler =>
      val blockHeaders =
        headersResult.drop(
          ChainUnitTest.FIRST_POW_CHANGE - ChainUnitTest.FIRST_BLOCK_HEIGHT)

      val firstBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(
          ChainUnitTest.FIRST_POW_CHANGE - 2,
          Pow.getBlockProof(ChainTestUtil.blockHeader562462),
          ChainTestUtil.blockHeader562462)

      val secondBlockHeaderDb = {
        val chainWork = firstBlockHeaderDb.chainWork + Pow.getBlockProof(
          ChainTestUtil.blockHeader562463)
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE - 1,
                                            chainWork,
                                            ChainTestUtil.blockHeader562463)
      }

      val thirdBlockHeaderDb = {
        val chainWork = secondBlockHeaderDb.chainWork + Pow.getBlockProof(
          ChainTestUtil.blockHeader562464)
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE,
                                            chainWork,
                                            ChainTestUtil.blockHeader562464)
      }

      /*
       * We need to insert one block before the first POW check because it is used on the next
       * POW check. We then need to insert the next to blocks to circumvent a POW check since
       * that would require we have an old block in the Blockchain that we don't have.
       */
      val firstThreeBlocks =
        Vector(firstBlockHeaderDb, secondBlockHeaderDb, thirdBlockHeaderDb)

      val createdF = chainHandler.blockHeaderDAO.createAll(firstThreeBlocks)

      createdF.flatMap { _ =>
        val blockchain = Blockchain.fromHeaders(firstThreeBlocks.reverse)
        val handler = chainHandler.copy(blockchains = Vector(blockchain))

        // Takes way too long to do all blocks
        val blockHeadersToTest = blockHeaders.tail
          .take(
            (2 * chainHandler.chainConfig.chain.difficultyChangeInterval + 1))

        val processedF = handler.processHeaders(blockHeadersToTest)

        for {
          ch <- processedF
          bestHash <- ch.getBestBlockHash
        } yield assert(bestHash == blockHeadersToTest.last.hashBE)
      }
  }

  it must "have getBestBlockHash return the header with the most work, not the highest" in {
    tempHandler: ChainHandler =>
      val dummyHeader =
        BlockHeaderDbHelper.fromBlockHeader(1,
                                            BigInt(0),
                                            ChainTestUtil.blockHeader562462)

      val highestHeader =
        BlockHeaderDbHelper.fromBlockHeader(2,
                                            BigInt(0),
                                            ChainTestUtil.blockHeader562463)

      val headerWithMostWork =
        BlockHeaderDbHelper.fromBlockHeader(1,
                                            BigInt(1000),
                                            ChainTestUtil.blockHeader562464)

      val tallestBlockchain =
        Blockchain(Vector(highestHeader, dummyHeader, genesis))
      val mostWorkChain = Blockchain(Vector(headerWithMostWork, genesis))

      val chainHandler =
        tempHandler.copy(blockchains = Vector(tallestBlockchain, mostWorkChain))

      for {
        hash <- chainHandler.getBestBlockHash()
      } yield {
        assert(hash == headerWithMostWork.blockHeader.hashBE)
      }
  }

  it must "be able to process and fetch real headers from mainnet" in {
    chainHandler: ChainHandler =>
      val blockHeaders =
        headersResult.drop(
          ChainUnitTest.FIRST_POW_CHANGE - ChainUnitTest.FIRST_BLOCK_HEIGHT)

      val firstBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE - 2,
                                            BigInt(0),
                                            ChainTestUtil.blockHeader562462)

      val secondBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE - 1,
                                            BigInt(0),
                                            ChainTestUtil.blockHeader562463)

      val thirdBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE,
                                            BigInt(0),
                                            ChainTestUtil.blockHeader562464)

      /*
       * We need to insert one block before the first POW check because it is used on the next
       * POW check. We then need to insert the next to blocks to circumvent a POW check since
       * that would require we have an old block in the Blockchain that we don't have.
       */
      val firstThreeBlocks =
        Vector(firstBlockHeaderDb, secondBlockHeaderDb, thirdBlockHeaderDb)

      val createdF = chainHandler.blockHeaderDAO.createAll(firstThreeBlocks)

      createdF.flatMap { _ =>
        val blockchain = Blockchain.fromHeaders(firstThreeBlocks.reverse)
        val handler = ChainHandler(chainHandler.blockHeaderDAO,
                                   chainHandler.filterHeaderDAO,
                                   chainHandler.filterDAO,
                                   blockchain)
        val processorF = Future.successful(handler)
        // Takes way too long to do all blocks
        val blockHeadersToTest = blockHeaders.tail
          .take(
            (2 * chainHandler.chainConfig.chain.difficultyChangeInterval + 1).toInt)

        processHeaders(processorF = processorF,
                       headers = blockHeadersToTest,
                       height = ChainUnitTest.FIRST_POW_CHANGE + 1)
      }
  }

  it must "properly recalculate chain work" in { tempHandler: ChainHandler =>
    val headersWithNoWork = Vector(
      BlockHeaderDbHelper.fromBlockHeader(3,
                                          BigInt(0),
                                          ChainTestUtil.blockHeader562464),
      BlockHeaderDbHelper.fromBlockHeader(2,
                                          BigInt(0),
                                          ChainTestUtil.blockHeader562463),
      BlockHeaderDbHelper.fromBlockHeader(1,
                                          BigInt(0),
                                          ChainTestUtil.blockHeader562462)
    )

    val blockchain = Blockchain(headersWithNoWork :+ genesis)

    val chainHandler = tempHandler.copy(blockchains = Vector(blockchain))

    for {
      _ <- chainHandler.blockHeaderDAO.createAll(headersWithNoWork)
      isMissingWork <- chainHandler.isMissingChainWork
      _ = assert(isMissingWork)
      newHandler <- chainHandler.recalculateChainWork
      headerDb <- newHandler.getBestBlockHeader()
    } yield {
      assert(headerDb.height == headersWithNoWork.head.height)
      assert(
        newHandler.blockchains.head
          .groupBy(_.hashBE)
          .forall(_._2.size == 1))
      assert(headerDb.hashBE == headersWithNoWork.head.hashBE)
      assert(headerDb.chainWork == BigInt(12885098501L))
    }
  }

}
