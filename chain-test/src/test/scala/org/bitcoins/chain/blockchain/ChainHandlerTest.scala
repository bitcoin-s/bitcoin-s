package org.bitcoins.chain.blockchain

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.crypto.{
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  ECPrivateKey
}
import org.bitcoins.core.gcs.{BlockFilter, FilterHeader, FilterType}
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.chain.fixture.ChainFixtureTag
import org.bitcoins.testkit.chain.{
  BlockHeaderHelper,
  ChainTestUtil,
  ChainUnitTest
}
import org.bitcoins.testkit.util.{FileUtil, ScalaTestUtil}
import org.scalatest.{Assertion, FutureOutcome}
import play.api.libs.json.Json

import scala.concurrent.Future

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

  val source = FileUtil.getFileAsSource("block_headers.json")
  val arrStr = source.getLines.next
  source.close()

  import org.bitcoins.rpc.serializers.JsonReaders.BlockHeaderReads
  val headersResult = Json.parse(arrStr).validate[Vector[BlockHeader]].get

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
      val blockHeaders =
        headersResult.drop(
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

  it must "benchmark ChainHandler.processHeaders()" in {
    chainHandler: ChainHandler =>
      val blockHeaders =
        headersResult.drop(
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

  it must "get the highest filter header" in { chainHandler: ChainHandler =>
    {
      val firstFilterHeader = FilterHeader(
        filterHash =
          DoubleSha256Digest.fromBytes(ECPrivateKey.freshPrivateKey.bytes),
        prevHeaderHash = DoubleSha256Digest.empty)
      for {
        empty <- chainHandler.getFilterHeadersAtHeight(0)
        block <- chainHandler.getHeadersAtHeight(0)
        _ <- chainHandler.processFilterHeader(firstFilterHeader,
                                              block.head.hashBE)
        count <- chainHandler.getFilterHeaderCount
        first <- chainHandler.getFilterHeader(block.head.hashBE)
        vec <- chainHandler.getFilterHeadersAtHeight(count)
      } yield {
        assert(empty.isEmpty)
        assert(first.nonEmpty)
        assert(vec.nonEmpty)
        assert(Vector(first.get) == vec)
        assert(first.get.hashBE == firstFilterHeader.hash.flip)
        assert(first.get.filterHashBE == firstFilterHeader.filterHash.flip)
        assert(
          first.get.previousFilterHeaderBE == firstFilterHeader.prevHeaderHash.flip)
        assert(first.get.blockHashBE == block.head.hashBE)
        assert(first.get.height == 0)
        assert(first.get.filterHeader == firstFilterHeader)
      }
    }
  }

  it must "NOT create a filter header for an unknown block" in {
    chainHandler: ChainHandler =>
      {
        val firstFilterHeader = FilterHeader(
          filterHash =
            DoubleSha256Digest.fromBytes(ECPrivateKey.freshPrivateKey.bytes),
          prevHeaderHash = DoubleSha256Digest.empty)
        val newChainHandlerF = chainHandler.processFilterHeader(
          firstFilterHeader,
          DoubleSha256DigestBE.fromBytes(ECPrivateKey.freshPrivateKey.bytes))
        recoverToSucceededIf[UnknownBlockHash](newChainHandlerF)
      }
  }

  it must "get the highest filter" in { chainHandler: ChainHandler =>
    {
      for {
        empty <- chainHandler.getFilterCount
        blockHashBE <- chainHandler.getHeadersAtHeight(0).map(_.head.hashBE)
        golombFilter = BlockFilter.fromHex("017fa880", blockHashBE.flip)
        firstFilter = CompactFilterMessage(blockHash = blockHashBE.flip,
                                           filter = golombFilter)
        firstFilterHeader = FilterHeader(filterHash = golombFilter.hash,
                                         prevHeaderHash =
                                           DoubleSha256Digest.empty)
        newChainHandler <- chainHandler.processFilterHeader(firstFilterHeader,
                                                            blockHashBE)
        _ <- chainHandler.processFilter(firstFilter)
        count <- newChainHandler.getFilterCount
        first <- newChainHandler.getFiltersAtHeight(count).map(_.headOption)
      } yield {
        assert(empty == 0)
        assert(first.nonEmpty)
        assert(first.get.hashBE == golombFilter.hash.flip)
        assert(first.get.height == 0)
        assert(first.get.blockHashBE == blockHashBE)
        assert(first.get.filterType == FilterType.Basic)
        assert(first.get.golombFilter == golombFilter)
      }
    }
  }

  it must "NOT create an unknown filter" in { chainHandler: ChainHandler =>
    {
      val unknownHashF = for {
        blockHashBE <- chainHandler.getHeadersAtHeight(0).map(_.head.hashBE)
        golombFilter = BlockFilter.fromHex("017fa880", blockHashBE.flip)
        firstFilter = CompactFilterMessage(blockHash = blockHashBE.flip,
                                           filter = golombFilter)
        firstFilterHeader = FilterHeader(
          filterHash =
            DoubleSha256Digest.fromBytes(ECPrivateKey.freshPrivateKey.bytes),
          prevHeaderHash = DoubleSha256Digest.empty)
        newChainHandler <- chainHandler.processFilterHeader(firstFilterHeader,
                                                            blockHashBE)
        process <- newChainHandler.processFilter(firstFilter)
      } yield {
        process
      }
      recoverToSucceededIf[UnknownFilterHash](unknownHashF)
    }
  }

  it must "NOT create a filter of an unknown block" in {
    chainHandler: ChainHandler =>
      {
        val blockHashBE =
          DoubleSha256DigestBE.fromBytes(ECPrivateKey.freshPrivateKey.bytes)
        val golombFilter = BlockFilter.fromHex("017fa880", blockHashBE.flip)
        val firstFilter = CompactFilterMessage(blockHash = blockHashBE.flip,
                                               filter = golombFilter)
        val firstFilterHeader = FilterHeader(filterHash = golombFilter.hash,
                                             prevHeaderHash =
                                               DoubleSha256Digest.empty)
        val unknownBlockF = for {
          realBlockHashBE <- chainHandler
            .getHeadersAtHeight(0)
            .map(_.head.hashBE)
          newChainHandler <- chainHandler.processFilterHeader(firstFilterHeader,
                                                              blockHashBE)
        } yield {
          assert(realBlockHashBE != blockHashBE)
          newChainHandler
        }
        recoverToSucceededIf[UnknownBlockHash](unknownBlockF)
      }
  }

  it must "create a filter checkpoint map" in { chainHandler: ChainHandler =>
    for {
      realBlockHashBE <- chainHandler.getHeadersAtHeight(0).map(_.head.hashBE)
      filterHashBE = DoubleSha256DigestBE.fromBytes(
        ECPrivateKey.freshPrivateKey.bytes)
      newChainHandler <- chainHandler.processCheckpoint(filterHashBE,
                                                        realBlockHashBE)
    } yield {
      assert(
        newChainHandler
          .asInstanceOf[ChainHandler]
          .blockFilterCheckpoints == Map(realBlockHashBE -> filterHashBE))
    }
  }

  it must "generate a range for a block filter query" in {
    chainHandler: ChainHandler =>
      for {
        bestBlock <- chainHandler.getBestBlockHeader
        bestBlockHashBE = bestBlock.hashBE
        rangeOpt <- chainHandler.nextHeaderBatchRange(
          DoubleSha256DigestBE.empty,
          1)
      } yield {
        assert(rangeOpt.nonEmpty)
        assert(rangeOpt.get._1 == 0)
        assert(rangeOpt.get._2 == bestBlockHashBE.flip)
      }
  }

  final def processHeaders(
      processorF: Future[ChainApi],
      headers: Vector[BlockHeader],
      height: Int): Future[Assertion] = {
    val processedHeadersF = processorF.flatMap(_.processHeaders(headers))

    def loop(
        remainingHeaders: Vector[BlockHeader],
        height: Int,
        accum: Vector[Future[Assertion]]): Vector[Future[Assertion]] = {
      remainingHeaders match {
        case header +: headersTail =>
          val getHeaderF = processedHeadersF.flatMap(_.getHeader(header.hashBE))
          val expectedBlockHeaderDb =
            BlockHeaderDbHelper.fromBlockHeader(height, header)
          val assertionF =
            getHeaderF.map(headerOpt =>
              assert(headerOpt.contains(expectedBlockHeaderDb)))
          val newAccum = accum.:+(assertionF)
          loop(headersTail, height + 1, newAccum)
        case Vector() =>
          accum
      }
    }

    val vecFutAssert: Vector[Future[Assertion]] =
      loop(headers, height, Vector.empty)

    ScalaTestUtil.toAssertF(vecFutAssert)
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
