package org.bitcoins.chain.blockchain

import akka.actor.ActorSystem
import org.bitcoins.chain.models.BlockHeaderDbHelper
import org.bitcoins.chain.util.{ChainFixtureTag, ChainUnitTest}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.FileUtil
import org.bitcoins.db.NetworkDb
import org.bitcoins.testkit.chain.{BlockHeaderHelper, ChainTestUtil}
import org.scalatest.{Assertion, FutureOutcome}
import play.api.libs.json.Json

import scala.concurrent.Future

class ChainHandlerTest extends ChainUnitTest {

  override type FixtureParam = ChainHandler

  override implicit val system = ActorSystem("ChainUnitTest")

  override val defaultTag: ChainFixtureTag = ChainFixtureTag.GenisisChainHandler

  override lazy val networkDb: NetworkDb = NetworkDb.MainNetDbConfig

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainHandler(test)

  behavior of "ChainHandler"

  it must "process a new valid block header, and then be able to fetch that header" in {
    chainHandler: ChainHandler =>
      val newValidHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
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
        headersResult.get.drop(FIRST_POW_CHANGE - FIRST_BLOCK_HEIGHT)

      val firstBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(FIRST_POW_CHANGE - 2,
                                            ChainTestUtil.blockHeader562462)

      val secondBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(FIRST_POW_CHANGE - 1,
                                            ChainTestUtil.blockHeader562463)

      val thirdBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(FIRST_POW_CHANGE,
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
              (2 * chainHandler.chainParams.difficultyChangeInterval + 1).toInt)
            .toList

          processHeaders(processorF = processorF,
            remainingHeaders = blockHeadersToTest,
            height = FIRST_POW_CHANGE + 1)
        }
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
