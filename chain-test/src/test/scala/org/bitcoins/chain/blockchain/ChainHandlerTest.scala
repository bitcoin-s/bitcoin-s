package org.bitcoins.chain.blockchain

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  BlockHeaderDb,
  BlockHeaderDbHelper
}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.FileUtil
import org.bitcoins.testkit.chain.fixture.ChainFixtureTag
import org.bitcoins.testkit.chain.{
  BlockHeaderHelper,
  ChainTestUtil,
  ChainUnitTest
}
import org.scalatest.{Assertion, FutureOutcome}
import play.api.libs.json.Json

import scala.concurrent.Future
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig

class ChainHandlerTest extends ChainUnitTest {

  override type FixtureParam = ChainHandler

  implicit override val system = ActorSystem("ChainUnitTest")

  // we're working with mainnet data
  implicit override lazy val appConfig: ChainAppConfig = {
    import BitcoinSTestAppConfig.ProjectType

    val memoryDb =
      BitcoinSTestAppConfig.configWithMemoryDb(Some(ProjectType.Chain))
    mainnetAppConfig.withOverrides(memoryDb)
  }

  override val defaultTag: ChainFixtureTag = ChainFixtureTag.GenisisChainHandler

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainHandler(test)

  val genesis = ChainUnitTest.genesisHeaderDb
  behavior of "ChainHandler"

  it must "process a new valid block header, and then be able to fetch that header" in {
    chainHandler: ChainHandler =>
      val newValidHeader =
        BlockHeaderHelper.buildNextHeader(ChainUnitTest.genesisHeaderDb)
      val processedHeaderF =
        chainHandler.processHeader(newValidHeader.blockHeader)

      val foundHeaderF =
        processedHeaderF.flatMap(_.getHeader(newValidHeader.hashBE))

      foundHeaderF.map(found => assert(found.get == newValidHeader))
  }

  it must "have an in-order seed" in { _ =>
    val source = FileUtil.getFileAsSource("block_headers.json")
    val arrStr = source.getLines.next
    source.close()

    import org.bitcoins.rpc.serializers.JsonReaders.BlockHeaderReads
    val headersResult = Json.parse(arrStr).validate[Vector[BlockHeader]]
    if (headersResult.isError) {
      fail(headersResult.toString)
    }

    val blockHeaders = headersResult.get

    blockHeaders.reduce[BlockHeader] {
      case (prev, next) =>
        assert(next.previousBlockHashBE == prev.hashBE)
        next
    }

    succeed
  }

  it must "be able to process and fetch real headers from mainnet" in {
    chainHandler: ChainHandler =>
      val source = FileUtil.getFileAsSource("block_headers.json")
      val arrStr = source.getLines.next
      source.close()

      import org.bitcoins.rpc.serializers.JsonReaders.BlockHeaderReads
      val headersResult = Json.parse(arrStr).validate[Vector[BlockHeader]]
      if (headersResult.isError) {
        fail(headersResult.toString)
      }

      val blockHeaders =
        headersResult.get.drop(
          ChainUnitTest.FIRST_POW_CHANGE - ChainUnitTest.FIRST_BLOCK_HEIGHT)

      val firstBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE - 2,
                                            ChainTestUtil.blockHeader562462)

      val secondBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE - 1,
                                            ChainTestUtil.blockHeader562463)

      val thirdBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE,
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
        val handler = ChainHandler(chainHandler.blockHeaderDAO, blockchain)
        val processorF = Future.successful(handler)
        // Takes way too long to do all blocks
        val blockHeadersToTest = blockHeaders.tail
          .take(
            (2 * chainHandler.chainConfig.chain.difficultyChangeInterval + 1).toInt)
          .toList

        processHeaders(processorF = processorF,
                       remainingHeaders = blockHeadersToTest,
                       height = ChainUnitTest.FIRST_POW_CHANGE + 1)
      }
  }

  it must "handle a very basic reorg where one chain is one block behind the best chain" in {
    chainHandler: ChainHandler =>
      val reorgFixtureF = buildChainHandlerCompetingHeaders(chainHandler)
      val chainHandlerCF = reorgFixtureF.map(_.chainApi)
      // header B, best hash ATM
      val newHeaderBF = reorgFixtureF.map(_.headerDb1)
      // header C, same height as B but was seen later
      val newHeaderCF = reorgFixtureF.map(_.headerDb2)

      // check that header B is the leader
      val assertBBestHashF = for {
        chainHandler <- chainHandlerCF
        headerB <- newHeaderBF
        bestHash <- chainHandler.getBestBlockHash
      } yield {
        assert(bestHash == headerB.hashBE)
      }

      // build a new header D off of C which was seen later
      // but has the same height as B
      val newHeaderDF =
        newHeaderCF.map(h => BlockHeaderHelper.buildNextHeader(h))

      val chainHandlerDF = for {
        _ <- assertBBestHashF
        newHeaderD <- newHeaderDF
        chainHandler <- chainHandlerCF
        chainHandlerD <- chainHandler.processHeader(newHeaderD.blockHeader)
      } yield chainHandlerD

      for {
        chainHandler <- chainHandlerDF
        newHeaderD <- newHeaderDF
        hash <- chainHandler.getBestBlockHash
      } yield {
        // assert that header D overtook header B
        assert(hash == newHeaderD.hashBE)
      }
  }

  it must "NOT reorg to a shorter chain that just received a new block" in {
    chainHandler: ChainHandler =>
      val reorgFixtureF = buildChainHandlerCompetingHeaders(chainHandler)
      val chainHandlerCF = reorgFixtureF.map(_.chainApi)
      val newHeaderBF = reorgFixtureF.map(_.headerDb1)
      val newHeaderCF = reorgFixtureF.map(_.headerDb2)

      //we are going to generate two new blocks on chain C
      val chainHandlerEWithHeaderF: Future[(ChainApi, BlockHeaderDb)] = for {
        newHeaderC <- newHeaderCF
        chainHandler <- chainHandlerCF
        headerD = BlockHeaderHelper.buildNextHeader(newHeaderC)
        headerE = BlockHeaderHelper.buildNextHeader(headerD)
        chainHandlerE <- chainHandler.processHeaders(
          Vector(headerD.blockHeader, headerE.blockHeader))
      } yield (chainHandlerE, headerE)

      val chainHandlerEF = chainHandlerEWithHeaderF.map(_._1)

      val headerEF = chainHandlerEWithHeaderF.map(_._2)
      //now we are going to attempt to generate a block on top of B
      //we should _not_ reorg to a new best tip after adding block F ontop of B
      //the best hash should still be header E's best hash.

      val chainHandlerFF = for {
        chainHandler <- chainHandlerEF
        headerB <- newHeaderBF
        headerF = BlockHeaderHelper.buildNextHeader(headerB)
        chainHandlerF <- chainHandler.processHeader(headerF.blockHeader)
      } yield chainHandlerF

      for {
        chainHandlerF <- chainHandlerFF
        headerE <- headerEF
        bestHash <- chainHandlerF.getBestBlockHash
      } yield assert(bestHash == headerE.hashBE)
  }

  final def processHeaders(
      processorF: Future[ChainHandler],
      remainingHeaders: List[BlockHeader],
      height: Int): Future[Assertion] = {
    remainingHeaders match {
      case header :: headersTail =>
        val newProcessorF = processorF.flatMap(_.processHeader(header))
        val getHeaderF = newProcessorF.flatMap(_.getHeader(header.hashBE))
        val expectedBlockHeaderDb =
          BlockHeaderDbHelper.fromBlockHeader(height, header)
        val assertionF =
          getHeaderF.map(tips => assert(tips.contains(expectedBlockHeaderDb)))
        assertionF.flatMap(_ =>
          processHeaders(newProcessorF, headersTail, height = height + 1))
      case Nil => succeed
    }
  }

  /** Builds two competing headers that are built from the same parent */
  private def buildCompetingHeaders(
      parent: BlockHeaderDb): (BlockHeader, BlockHeader) = {
    val newHeaderB =
      BlockHeaderHelper.buildNextHeader(parent)

    val newHeaderC =
      BlockHeaderHelper.buildNextHeader(parent)

    (newHeaderB.blockHeader, newHeaderC.blockHeader)
  }

  case class ReorgFixture(
      chainApi: ChainApi,
      headerDb1: BlockHeaderDb,
      headerDb2: BlockHeaderDb,
      oldBestBlockHeader: BlockHeaderDb) {
    lazy val header1: BlockHeader = headerDb1.blockHeader
    lazy val header2: BlockHeader = headerDb2.blockHeader
  }

  /** Builds two competing headers off of the [[ChainHandler.getBestBlockHash best chain tip]] */
  private def buildChainHandlerCompetingHeaders(
      chainHandler: ChainHandler): Future[ReorgFixture] = {
    for {
      oldBestTip <- chainHandler.getBestBlockHeader
      (newHeaderB, newHeaderC) = buildCompetingHeaders(oldBestTip)
      newChainApi <- chainHandler.processHeaders(Vector(newHeaderB, newHeaderC))
      newHeaderDbB <- newChainApi.getHeader(newHeaderB.hashBE)
      newHeaderDbC <- newChainApi.getHeader(newHeaderC.hashBE)
    } yield {
      ReorgFixture(newChainApi, newHeaderDbB.get, newHeaderDbC.get, oldBestTip)
    }
  }
}
