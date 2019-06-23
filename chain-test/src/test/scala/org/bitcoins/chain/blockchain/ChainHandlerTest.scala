package org.bitcoins.chain.blockchain

import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDbHelper}
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
import org.bitcoins.testkit.BitcoinSAppConfig

class ChainHandlerTest extends ChainUnitTest {

  override type FixtureParam = ChainHandler

  implicit override val system = ActorSystem("ChainUnitTest")

  // we're working with mainnet data
  implicit override lazy val appConfig: ChainAppConfig = {
    val memoryDb = BitcoinSAppConfig.configWithMemoryDb(
      Some(BitcoinSAppConfig.ProjectType.Chain))
    mainnetAppConfig.withOverrides(memoryDb)
  }

  override val defaultTag: ChainFixtureTag = ChainFixtureTag.GenisisChainHandler

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainHandler(test)

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
        val processorF = Future.successful(chainHandler)
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
      val newHeaderB =
        BlockHeaderHelper.buildNextHeader(ChainUnitTest.genesisHeaderDb)

      //this builds a blockchain that is A -> B
      val chainHandlerB =
        chainHandler.processHeader(header = newHeaderB.blockHeader)

      //we have connected one header off of the genesis block
      //now we are going to build two headers off of the genesis block
      //that should for a reorg
      val bestHashB = chainHandlerB.flatMap(_.getBestBlockHash)

      val bIsBestHashF =
        bestHashB.map(hash => assert(hash == newHeaderB.hashBE))

      val newHeaderC =
        BlockHeaderHelper.buildNextHeader(ChainUnitTest.genesisHeaderDb)

      val newHeaderD = BlockHeaderHelper.buildNextHeader(newHeaderC)

      val reorgHeaders = Vector(newHeaderC, newHeaderD).map(_.blockHeader)

      val chainHandlerCD = bIsBestHashF.flatMap { _ =>
        chainHandlerB.flatMap(_.processHeaders(reorgHeaders))
      }

      for {
        chainHandler <- chainHandlerCD
        hash <- chainHandler.getBestBlockHash
      } yield {
        assert(hash == newHeaderD.hashBE)
      }
  }

  it must "handle a reorg where both chains are at the exact same height when a new block comes in" in {
    chainHandler: ChainHandler =>
      val newHeaderB =
        BlockHeaderHelper.buildNextHeader(ChainUnitTest.genesisHeaderDb)

      val newHeaderC =
        BlockHeaderHelper.buildNextHeader(ChainUnitTest.genesisHeaderDb)

      //this builds a blockchain that is A -> B
      val chainHandlerB =
        chainHandler.processHeader(header = newHeaderB.blockHeader)

      //this means we now have a chain where both B and C are built on top of A
      //and they are competing with each other. Since we saw B first
      //our implementation currently favors that as the best hash
      val chainHandlerC =
        chainHandlerB.flatMap(_.processHeader(newHeaderC.blockHeader))

      val bestHashB = for {
        chainHandler <- chainHandlerC
        bestHash <- chainHandler.getBestBlockHash
      } yield assert(bestHash == newHeaderB.hashBE)

      //let's generate block D that is built ontop of C
      val newHeaderD = BlockHeaderHelper.buildNextHeader(newHeaderC)

      val chainHandlerD =
        chainHandlerC.flatMap(_.processHeader(newHeaderD.blockHeader))

      //now our best hash should be D
      for {
        chainHandler <- chainHandlerD
        bestHashD <- chainHandler.getBestBlockHash
        _ <- bestHashB
      } yield assert(bestHashD == newHeaderD.hashBE)
  }

  final def processHeaders(
      processorF: Future[ChainHandler],
      remainingHeaders: List[BlockHeader],
      height: Long): Future[Assertion] = {
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
}
