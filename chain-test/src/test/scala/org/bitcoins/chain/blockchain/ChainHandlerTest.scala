package org.bitcoins.chain.blockchain

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.gcs.{BlockFilter, FilterHeader}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto.{
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  ECPrivateKey
}
import org.bitcoins.testkit.chain.fixture.ChainFixtureTag
import org.bitcoins.testkit.chain.{
  BlockHeaderHelper,
  ChainDbUnitTest,
  ChainTestUtil,
  ChainUnitTest
}
import org.bitcoins.testkit.util.{FileUtil, ScalaTestUtil}
import org.scalatest.{Assertion, FutureOutcome}
import play.api.libs.json.Json

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.io.BufferedSource

class ChainHandlerTest extends ChainDbUnitTest {

  override type FixtureParam = ChainHandler

  implicit override val system = ActorSystem("ChainUnitTest")

  // we're working with mainnet data
  implicit override lazy val appConfig: ChainAppConfig = mainnetAppConfig

  val source: BufferedSource = FileUtil.getFileAsSource("block_headers.json")
  val arrStr: String = source.getLines.next
  source.close()

  import org.bitcoins.commons.serializers.JsonReaders.BlockHeaderReads

  val headersResult: Vector[BlockHeader] =
    Json.parse(arrStr).validate[Vector[BlockHeader]].get

  override val defaultTag: ChainFixtureTag = ChainFixtureTag.GenisisChainHandler

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainHandler(test)

  val genesis: BlockHeaderDb = ChainUnitTest.genesisHeaderDb
  behavior of "ChainHandler"

  val nextBlockHeader: BlockHeader =
    BlockHeader(
      version = Int32(1),
      previousBlockHash = ChainUnitTest.genesisHeaderDb.hashBE.flip,
      merkleRootHash = DoubleSha256Digest.empty,
      time = UInt32(1231006505),
      nBits = UInt32(545259519),
      nonce = UInt32(2083236893)
    )

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

  it must "have an in-order seed" in { _ =>
    val source = FileUtil.getFileAsSource("block_headers.json")
    val arrStr = source.getLines.next
    source.close()

    import org.bitcoins.commons.serializers.JsonReaders.BlockHeaderReads
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

  it must "not fail ChainHandler.processHeaders() with empty headers collection" in {
    chainHandler: ChainHandler =>
      for {
        _ <- chainHandler.processHeaders(Vector.empty)
      } yield succeed
  }

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

  // B
  // C -> D
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
        hash <- chainHandler.getBestBlockHash()
      } yield {
        // assert that header D overtook header B
        assert(hash == newHeaderD.hashBE)
      }
  }

  // G -> A -> B
  // G -> C -> D -> E
  it must "handle a reorg where one chain is two blocks behind the best chain" in {
    chainHandler: ChainHandler =>
      for {
        genesis <- chainHandler.getBestBlockHeader()

        oldFirst = BlockHeaderHelper.buildNextHeader(genesis)
        oldSecond = BlockHeaderHelper.buildNextHeader(oldFirst)
        startChain = Vector(oldFirst, oldSecond)

        toBeReorged <-
          chainHandler.processHeaders(startChain.map(_.blockHeader))
        oldTip <- toBeReorged.getBestBlockHeader()
        _ = assert(oldTip.hashBE == oldSecond.hashBE)

        newFirst = BlockHeaderHelper.buildNextHeader(genesis)
        newSecond = BlockHeaderHelper.buildNextHeader(newFirst)
        third = BlockHeaderHelper.buildNextHeader(newSecond)
        newChain = Vector(newFirst, newSecond, third)

        reorged <- chainHandler.processHeaders(newChain.map(_.blockHeader))
        newTip <- reorged.getBestBlockHeader()
      } yield {
        assert(newTip.hashBE == third.hashBE)
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
      for {
        count <- chainHandler.getFilterHeaderCount
        genesisFilterHeader <- chainHandler.getFilterHeadersAtHeight(count)
      } yield {
        assert(genesisFilterHeader.size == 1)
        assert(
          genesisFilterHeader.contains(ChainUnitTest.genesisFilterHeaderDb))
        assert(
          genesisFilterHeader.head.filterHeader == ChainUnitTest.genesisFilterHeaderDb.filterHeader)
        assert(count == 0)
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
        count <- chainHandler.getFilterCount
        genesisFilter <- chainHandler.getFiltersAtHeight(count)
      } yield {
        assert(count == 0)
        assert(genesisFilter.contains(ChainUnitTest.genesisFilterDb))
        assert(
          genesisFilter.head.golombFilter == ChainUnitTest.genesisFilterDb.golombFilter)
      }
    }
  }

  it must "NOT create an unknown filter" in { chainHandler: ChainHandler =>
    {
      val unknownHashF = for {
        _ <- chainHandler.processHeader(nextBlockHeader)
        blockHashBE <- chainHandler.getHeadersAtHeight(1).map(_.head.hashBE)
        golombFilter = BlockFilter.fromHex("017fa880", blockHashBE.flip)
        firstFilter = CompactFilterMessage(blockHash = blockHashBE.flip,
                                           filter = golombFilter)
        firstFilterHeader = FilterHeader(
          filterHash =
            DoubleSha256Digest.fromBytes(ECPrivateKey.freshPrivateKey.bytes),
          prevHeaderHash = ChainUnitTest.genesisFilterHeaderDb.hashBE.flip)
        newChainHandler <-
          chainHandler.processFilterHeader(firstFilterHeader, blockHashBE)
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
        val firstFilterHeader = FilterHeader(filterHash = golombFilter.hash,
                                             prevHeaderHash =
                                               DoubleSha256Digest.empty)
        val unknownBlockF = for {
          realBlockHashBE <-
            chainHandler
              .getHeadersAtHeight(0)
              .map(_.head.hashBE)
          newChainHandler <-
            chainHandler.processFilterHeader(firstFilterHeader, blockHashBE)
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
      filterHashBE =
        DoubleSha256DigestBE.fromBytes(ECPrivateKey.freshPrivateKey.bytes)
      newChainHandler <-
        chainHandler.processCheckpoint(filterHashBE, realBlockHashBE)
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
        bestBlock <- chainHandler.getBestBlockHeader()
        bestBlockHashBE = bestBlock.hashBE
        rangeOpt <-
          chainHandler.nextHeaderBatchRange(DoubleSha256DigestBE.empty, 1)
      } yield {
        assert(rangeOpt.nonEmpty)
        assert(rangeOpt.get._1 == 0)
        assert(rangeOpt.get._2 == bestBlockHashBE.flip)
      }
  }

  it must "generate a range for a block filter header query" in {
    chainHandler: ChainHandler =>
      for {
        bestBlock <- chainHandler.getBestBlockHeader()
        bestBlockHashBE = bestBlock.hashBE
        rangeOpt <-
          chainHandler.nextFilterHeaderBatchRange(DoubleSha256DigestBE.empty, 1)
      } yield {
        assert(rangeOpt.nonEmpty)
        assert(rangeOpt.get._1 == 0)
        assert(rangeOpt.get._2 == bestBlockHashBE.flip)
      }
  }

  it must "read compact filters for the database" in {
    chainHandler: ChainHandler =>
      for {
        bestBlock <- chainHandler.getBestBlockHeader()
        filterHeader <- chainHandler.getFilterHeader(bestBlock.hashBE)
        filter <- chainHandler.getFilter(bestBlock.hashBE)
      } yield {
        assert(filterHeader.isDefined)
        assert(filter.isDefined)
        assert(filterHeader.get.filterHashBE == filter.get.hashBE)
      }
  }

  it must "return the number of confirmations" in {
    chainHandler: ChainHandler =>
      for {
        bestBlockHashBE <- chainHandler.getBestBlockHash()
        confirmations <- chainHandler.getNumberOfConfirmations(bestBlockHashBE)
      } yield {
        assert(confirmations == Some(1))
      }
  }

  it must "return the height by block stamp" in { chainHandler: ChainHandler =>
    for {
      bestBlock <- chainHandler.getBestBlockHeader()
      stamp1 = BlockStamp.BlockHash(bestBlock.hashBE)
      stamp2 = BlockStamp.BlockHeight(bestBlock.height)
      stamp3 = BlockStamp.BlockTime(bestBlock.time)
      height1 <- chainHandler.getHeightByBlockStamp(stamp1)
      height2 <- chainHandler.getHeightByBlockStamp(stamp2)
      // TODO implement BlockTime
//      height3 <- chainHandler.getHeightByBlockStamp(stamp3)
    } yield {
      assert(height1 == height2)
//      assert(height1 == height3)
    }
  }

  it must "get the correct height from an epoch second" in {
    chainHandler: ChainHandler =>
      for {
        height <-
          chainHandler.epochSecondToBlockHeight(TimeUtil.currentEpochSecond)
      } yield {
        assert(height == 0)
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

  final def processHeaders(
      processorF: Future[ChainApi],
      headers: Vector[BlockHeader],
      height: Int): Future[Assertion] = {
    val processedHeadersF = processorF.flatMap(_.processHeaders(headers))

    @tailrec
    def loop(
        remainingHeaders: Vector[BlockHeader],
        prevHeaderDbOpt: Option[BlockHeaderDb],
        height: Int,
        accum: Vector[Future[Assertion]]): Vector[Future[Assertion]] = {
      remainingHeaders match {
        case header +: headersTail =>
          val getHeaderF = processedHeadersF.flatMap(_.getHeader(header.hashBE))

          val chainWork = prevHeaderDbOpt match {
            case None => Pow.getBlockProof(header)
            case Some(prevHeader) =>
              prevHeader.chainWork + Pow.getBlockProof(header)
          }

          val expectedBlockHeaderDb =
            BlockHeaderDbHelper.fromBlockHeader(height, chainWork, header)
          val assertionF =
            getHeaderF.map(headerOpt =>
              assert(headerOpt.contains(expectedBlockHeaderDb)))
          val newAccum = accum.:+(assertionF)
          loop(headersTail, Some(expectedBlockHeaderDb), height + 1, newAccum)
        case Vector() =>
          accum
      }
    }

    val vecFutAssert: Vector[Future[Assertion]] =
      loop(headers, None, height, Vector.empty)

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
      oldBestTip <- chainHandler.getBestBlockHeader()
      (newHeaderB, newHeaderC) = buildCompetingHeaders(oldBestTip)
      newChainApi <- chainHandler.processHeaders(Vector(newHeaderB, newHeaderC))
      newHeaderDbB <- newChainApi.getHeader(newHeaderB.hashBE)
      newHeaderDbC <- newChainApi.getHeader(newHeaderC.hashBE)
    } yield {
      ReorgFixture(newChainApi, newHeaderDbB.get, newHeaderDbC.get, oldBestTip)
    }
  }
}
