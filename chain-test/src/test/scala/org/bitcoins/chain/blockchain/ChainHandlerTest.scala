package org.bitcoins.chain.blockchain

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.pow.Pow
import org.bitcoins.chain.{ChainCallbacks, OnSyncFlagChanged}
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.chain.db.{
  BlockHeaderDb,
  BlockHeaderDbHelper,
  CompactFilterHeaderDb
}
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
  ChainUnitTest
}
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkitcore.chain.ChainTestUtil
import org.scalatest.{Assertion, Assertions, FutureOutcome}
import play.api.libs.json.Json

import scala.concurrent.Future

class ChainHandlerTest extends ChainDbUnitTest {

  override type FixtureParam = ChainHandler

  override val defaultTag: ChainFixtureTag =
    ChainFixtureTag.GenesisChainHandlerWithFilter

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainHandler(test)

  val genesis: BlockHeaderDb = ChainTestUtil.genesisHeaderDb
  behavior of "ChainHandler"

  val nextBlockHeader: BlockHeader =
    BlockHeader(
      version = Int32(1),
      previousBlockHash = ChainTestUtil.genesisHeaderDb.hashBE.flip,
      merkleRootHash = DoubleSha256Digest.empty,
      time = UInt32(1231006505),
      nBits = UInt32(545259519),
      nonce = UInt32(2083236893)
    )

  private def insertGenesisFilterHeaderAndFilter(
      chainHandler: ChainHandler): Future[Unit] = {
    for {
      _ <- chainHandler.processFilterHeader(
        ChainTestUtil.genesisFilterHeaderDb.filterHeader,
        ChainTestUtil.genesisHeaderDb.hashBE)
      _ <- chainHandler.processFilter(ChainTestUtil.genesisFilterMessage)
    } yield ()
  }

  it must "process a new valid block header, and then be able to fetch that header" in {
    chainHandler: ChainHandler =>
      val newValidHeader =
        BlockHeaderHelper.buildNextHeader(ChainTestUtil.genesisHeaderDb)
      val processedHeaderF =
        chainHandler.processHeader(newValidHeader.blockHeader)

      val foundHeaderF =
        processedHeaderF.flatMap(_.getHeader(newValidHeader.hashBE))

      foundHeaderF.map(found => assert(found.get == newValidHeader))
  }

  it must "have an in-order seed" in { _ =>
    val source = FileUtil.getFileAsSource("block_headers.json")
    val arrStr = source.getLines().next()
    source.close()

    import org.bitcoins.commons.serializers.JsonReaders.BlockHeaderReads
    val headersResult = Json.parse(arrStr).validate[Vector[BlockHeader]]
    if (headersResult.isError) {
      fail(headersResult.toString)
    }

    val blockHeaders = headersResult.get

    blockHeaders.reduce[BlockHeader] { case (prev, next) =>
      assert(next.previousBlockHashBE == prev.hashBE)
      next
    }

    succeed
  }

  it must "not fail ChainHandler.processHeaders() with empty headers collection" in {
    chainHandler: ChainHandler =>
      for {
        _ <- chainHandler.processHeaders(Vector.empty)
      } yield succeed
  }

  it must "fail if we give a header that cannot be connected to anything" in {
    chainHandler: ChainHandler =>
      val newHeader = BlockHeaderHelper.badPrevHash
      recoverToSucceededIf[RuntimeException] {
        for {
          result <- chainHandler.processHeader(newHeader)
        } yield result
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
        newHeaderB <- newHeaderBF
        bestHash <- chainHandler.getBestBlockHash()
        newHeaderC <- newHeaderCF
      } yield {
        ChainHandlerTest.checkReorgHeaders(header1 = newHeaderB,
                                           header2 = newHeaderC,
                                           bestHash = bestHash)
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

      // we are going to generate two new blocks on chain C
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
      // now we are going to attempt to generate a block on top of B
      // we should _not_ reorg to a new best tip after adding block F ontop of B
      // the best hash should still be header E's best hash.

      val chainHandlerFF = for {
        chainHandler <- chainHandlerEF
        headerB <- newHeaderBF
        headerF = BlockHeaderHelper.buildNextHeader(headerB)
        chainHandlerF <- chainHandler.processHeader(headerF.blockHeader)
      } yield chainHandlerF

      for {
        chainHandlerF <- chainHandlerFF
        headerE <- headerEF
        bestHash <- chainHandlerF.getBestBlockHash()
      } yield assert(bestHash == headerE.hashBE)
  }

  it must "get the highest filter header" in { chainHandler: ChainHandler =>
    {
      for {
        initFhCount <- chainHandler.getFilterHeaderCount()
        _ <- insertGenesisFilterHeaderAndFilter(chainHandler)
        count <- chainHandler.getFilterHeaderCount()
        genesisFilterHeader <- chainHandler.getFilterHeadersAtHeight(count)
      } yield {
        assert(initFhCount == 0)
        assert(genesisFilterHeader.size == 1)
        assert(
          genesisFilterHeader.contains(ChainTestUtil.genesisFilterHeaderDb))
        assert(
          genesisFilterHeader.head.filterHeader == ChainTestUtil.genesisFilterHeaderDb.filterHeader)
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

  it must "verify a batch of filter headers" in { chainHandler: ChainHandler =>
    val goodBatch = Vector(
      ChainTestUtil.genesisFilterHeaderDb,
      CompactFilterHeaderDb(
        hashBE = DoubleSha256DigestBE.fromHex(
          "000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f"),
        previousFilterHeaderBE = ChainTestUtil.genesisFilterHeaderDb.hashBE,
        height = 1,
        filterHashBE = DoubleSha256DigestBE.fromHex(
          "555152535455565758595a5b5c5d5e5f555152535455565758595a5b5c5d5e5f"),
        blockHashBE = DoubleSha256DigestBE.fromHex(
          "aaa1a2a3a4a5a6a7a8a9aaabacadaeafaaa1a2a3a4a5a6a7a8a9aaabacadaeaf")
      )
    )

    val invalidGenesisFilterHeaderBatch = Vector(
      ChainTestUtil.genesisFilterHeaderDb.copy(
        hashBE = ChainTestUtil.genesisFilterHeaderDb.previousFilterHeaderBE,
        previousFilterHeaderBE = ChainTestUtil.genesisFilterHeaderDb.hashBE
      )
    )

    val invalidFilterHeaderBatch = Vector(
      ChainTestUtil.genesisFilterHeaderDb.copy(height = 1)
    )

    val selfReferenceFilterHeaderBatch = Vector(
      ChainTestUtil.genesisFilterHeaderDb,
      CompactFilterHeaderDb(
        hashBE = DoubleSha256DigestBE.fromHex(
          "000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f"),
        previousFilterHeaderBE = DoubleSha256DigestBE.fromHex(
          "000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f"),
        height = 1,
        filterHashBE = DoubleSha256DigestBE.fromHex(
          "555152535455565758595a5b5c5d5e5f555152535455565758595a5b5c5d5e5f"),
        blockHashBE = DoubleSha256DigestBE.fromHex(
          "aaa1a2a3a4a5a6a7a8a9aaabacadaeafaaa1a2a3a4a5a6a7a8a9aaabacadaeaf")
      )
    )

    val unknownFilterHeaderBatch = Vector(
      ChainTestUtil.genesisFilterHeaderDb,
      CompactFilterHeaderDb(
        hashBE = DoubleSha256DigestBE.fromHex(
          "000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f"),
        previousFilterHeaderBE = DoubleSha256DigestBE.fromHex(
          "555152535455565758595a5b5c5d5e5f555152535455565758595a5b5c5d5e5f"),
        height = 1,
        filterHashBE = DoubleSha256DigestBE.fromHex(
          "555152535455565758595a5b5c5d5e5f555152535455565758595a5b5c5d5e5f"),
        blockHashBE = DoubleSha256DigestBE.fromHex(
          "aaa1a2a3a4a5a6a7a8a9aaabacadaeafaaa1a2a3a4a5a6a7a8a9aaabacadaeaf")
      )
    )

    for {
      _ <- chainHandler.verifyFilterHeaders(goodBatch)
      _ <- recoverToSucceededIf[IllegalArgumentException](
        chainHandler.verifyFilterHeaders(invalidGenesisFilterHeaderBatch))
      _ <- recoverToSucceededIf[IllegalArgumentException](
        chainHandler.verifyFilterHeaders(invalidFilterHeaderBatch))
      _ <- recoverToSucceededIf[IllegalArgumentException](
        chainHandler.verifyFilterHeaders(selfReferenceFilterHeaderBatch))
      _ <- recoverToSucceededIf[IllegalArgumentException](
        chainHandler.verifyFilterHeaders(unknownFilterHeaderBatch))
    } yield succeed
  }

  it must "get the highest filter" in { chainHandler: ChainHandler =>
    {
      for {
        count <- chainHandler.getFilterCount()
        _ <- insertGenesisFilterHeaderAndFilter(chainHandler)
        genesisFilter <- chainHandler.getFiltersAtHeight(0)
        count1 <- chainHandler.getFilterCount()
      } yield {
        assert(count == 0)
        assert(count1 == 0)
        assert(genesisFilter.contains(ChainTestUtil.genesisFilterDb))
        assert(
          genesisFilter.head.golombFilter == ChainTestUtil.genesisFilterDb.golombFilter)
      }
    }
  }

  it must "NOT create an unknown filter" in { chainHandler: ChainHandler =>
    {
      val unknownHashF = for {
        _ <- insertGenesisFilterHeaderAndFilter(chainHandler)
        _ <- chainHandler.processHeader(nextBlockHeader)
        blockHashBE <- chainHandler.getHeadersAtHeight(1).map(_.head.hashBE)
        golombFilter = BlockFilter.fromHex("017fa880", blockHashBE.flip)
        firstFilter = CompactFilterMessage(blockHash = blockHashBE.flip,
                                           filter = golombFilter)
        firstFilterHeader = FilterHeader(
          filterHash =
            DoubleSha256Digest.fromBytes(ECPrivateKey.freshPrivateKey.bytes),
          prevHeaderHash = ChainTestUtil.genesisFilterHeaderDb.hashBE.flip)
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

  it must "generate a range for a filter header query for the genesis block" in {
    chainHandler: ChainHandler =>
      val genesisHeader =
        chainHandler.chainConfig.chain.genesisBlock.blockHeader
      val assert1F = for {
        rangeOpt <-
          chainHandler.nextBlockHeaderBatchRange(prevStopHash =
                                                   DoubleSha256DigestBE.empty,
                                                 stopHash =
                                                   genesisHeader.hashBE,
                                                 batchSize = 1)
      } yield {
        assert(rangeOpt.nonEmpty)
        val marker = rangeOpt.get
        assert(marker.startHeight == 0)
        assert(marker.stopBlockHash == genesisHeader.hash)
      }

      // let's process a block header, and then be able to fetch that header as the last stopHash
      val blockHeaderDb = {
        BlockHeaderDbHelper.fromBlockHeader(height = 0,
                                            chainWork =
                                              Pow.getBlockProof(genesisHeader),
                                            bh = genesisHeader)
      }

      val blockHeader = BlockHeaderHelper.buildNextHeader(blockHeaderDb)
      val chainApi2 = assert1F.flatMap { _ =>
        chainHandler.processHeader(blockHeader.blockHeader)
      }

      for {
        chainApi <- chainApi2
        rangeOpt <-
          chainApi.nextBlockHeaderBatchRange(DoubleSha256DigestBE.empty,
                                             stopHash = blockHeader.hashBE,
                                             batchSize = 2)
      } yield {
        val marker = rangeOpt.get
        assert(rangeOpt.nonEmpty)
        assert(marker.startHeight == 0)
        assert(marker.stopBlockHash == blockHeader.hash)
      }
  }

  it must "generate the correct range of filter headers if a block header is reorged" in {
    chainHandler: ChainHandler =>
      val reorgFixtureF = buildChainHandlerCompetingHeaders(chainHandler)
      val chainHandlerF = reorgFixtureF.map(_.chainApi)
      val newHeaderBF = reorgFixtureF.map(_.headerDb1)
      val newHeaderCF = reorgFixtureF.map(_.headerDb2)
      val batchSize = 100

      // two competing headers B,C built off of A
      // first specify header B to be syncing filter headers from
      val assert0F = for {
        chainHandler <- chainHandlerF
        newHeaderB <- newHeaderBF
        newHeaderC <- newHeaderCF
        blockHeaderBatchOpt <- chainHandler.nextBlockHeaderBatchRange(
          prevStopHash = ChainTestUtil.regTestGenesisHeaderDb.hashBE,
          stopHash = newHeaderB.hashBE,
          batchSize = batchSize)
      } yield {
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        ChainHandlerTest.checkReorgHeaders(header1 = newHeaderB,
                                           header2 = newHeaderC,
                                           bestHash = marker.stopBlockHash.flip)
        assert(newHeaderB.height == marker.startHeight)
        assert(newHeaderB.hashBE == marker.stopBlockHash.flip)
      }

      // two competing headers B,C built off of A
      // first specify header C to be syncing filter headers from
      val assert1F = for {
        _ <- assert0F
        chainHandler <- chainHandlerF
        newHeaderB <- newHeaderBF
        newHeaderC <- newHeaderCF
        blockHeaderBatchOpt <- chainHandler.nextBlockHeaderBatchRange(
          prevStopHash = ChainTestUtil.regTestGenesisHeaderDb.hashBE,
          stopHash = newHeaderC.hashBE,
          batchSize = batchSize)
      } yield {
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        ChainHandlerTest.checkReorgHeaders(header1 = newHeaderB,
                                           header2 = newHeaderC,
                                           bestHash = marker.stopBlockHash.flip)
        assert(newHeaderC.height == marker.startHeight)
        assert(newHeaderC.hashBE == marker.stopBlockHash.flip)
      }

      // now let's build a new block header ontop of C and process it
      // when we call chainHandler.nextBlockHeaderBatchRange it
      // should be C's hash instead of B's hash
      for {
        _ <- assert1F
        chainHandler <- chainHandlerF
        headerC <- newHeaderCF
        headerD = BlockHeaderHelper.buildNextHeader(headerC)
        chainApiD <- chainHandler.processHeader(headerD.blockHeader)
        blockHeaderBatchOpt <- chainApiD.nextBlockHeaderBatchRange(
          prevStopHash = ChainTestUtil.regTestGenesisHeaderDb.hashBE,
          stopHash = headerD.hashBE,
          batchSize = batchSize)
        count <- chainApiD.getBlockCount()
      } yield {
        assert(count == 2)
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        assert(headerC.height == marker.startHeight)
        assert(headerD.hashBE == marker.stopBlockHash.flip)
      }

  }

  it must "return None for ChainHandler.nextBlockHeaderBatchRange if we are synced" in {
    chainHandler: ChainHandler =>
      val assert1F = for {
        bestBlockHash <- chainHandler.getBestBlockHash()
        rangeOpt <-
          chainHandler.nextBlockHeaderBatchRange(prevStopHash = bestBlockHash,
                                                 stopHash = bestBlockHash,
                                                 batchSize = 1)
      } yield {
        assert(rangeOpt.isEmpty, s"rangeOpt=$rangeOpt")
      }
      assert1F
  }

  it must "nextBlockHeaderBatchRange must honor the batchSize query" in {
    chainHandler: ChainHandler =>
      val reorgFixtureF = buildChainHandlerCompetingHeaders(chainHandler)
      val chainHandlerF = reorgFixtureF.map(_.chainApi)
      val newHeaderBF = reorgFixtureF.map(_.headerDb1)
      val newHeaderCF = reorgFixtureF.map(_.headerDb2)

      // two competing headers B,C built off of A
      // first specify header C to be syncing filter headers from
      val assert1F = for {
        chainHandler <- chainHandlerF
        newHeaderB <- newHeaderBF
        newHeaderC <- newHeaderCF
        blockHeaderBatchOpt <- chainHandler.nextBlockHeaderBatchRange(
          prevStopHash = ChainTestUtil.regTestGenesisHeaderDb.hashBE,
          stopHash = newHeaderC.hashBE,
          batchSize = 1)
      } yield {
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        ChainHandlerTest.checkReorgHeaders(header1 = newHeaderB,
                                           header2 = newHeaderC,
                                           bestHash = marker.stopBlockHash.flip)
        assert(marker.startHeight == 1)
        assert(newHeaderC.hashBE == marker.stopBlockHash.flip)
      }

      val headerDF = newHeaderCF.map(BlockHeaderHelper.buildNextHeader)
      // now let's build a new block header ontop of C and process it
      // when we call chainHandler.nextBlockHeaderBatchRange it
      // should be C's hash instead of D's hash due to batchSize
      val assert2F = for {
        _ <- assert1F
        chainHandler <- chainHandlerF
        headerC <- newHeaderCF
        headerD <- headerDF
        chainApiD <- chainHandler.processHeader(headerD.blockHeader)
        blockHeaderBatchOpt <- chainApiD.nextBlockHeaderBatchRange(
          prevStopHash = ChainTestUtil.regTestGenesisHeaderDb.hashBE,
          stopHash = headerD.hashBE,
          batchSize = 1)
      } yield {
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        assert(marker.startHeight == 1)
        assert(headerC.hashBE == marker.stopBlockHashBE)
      }

      val assert3F = for {
        _ <- assert2F
        chainHandler <- chainHandlerF
        headerD <- headerDF
        blockHeaderBatchOpt <- chainHandler.nextBlockHeaderBatchRange(
          prevStopHash = ChainTestUtil.regTestGenesisHeaderDb.hashBE,
          stopHash = headerD.hashBE,
          batchSize = 2)
      } yield {
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        assert(marker.startHeight == 1)
        assert(headerD.hashBE == marker.stopBlockHash.flip)
      }

      // must return None in the case of reorg scenario between prevStopHash / stopHash
      for {
        _ <- assert3F
        chainHandler <- chainHandlerF
        headerB <- newHeaderBF
        headerD <- headerDF
        // note headerB and headerD are not part of the same chain as D is built ontop of C
        blockHeaderBatchOpt <-
          chainHandler.nextBlockHeaderBatchRange(prevStopHash = headerB.hashBE,
                                                 stopHash = headerD.hashBE,
                                                 batchSize = 1)
      } yield {
        assert(blockHeaderBatchOpt.isEmpty)
      }
  }

  it must "generate the next range of block headers correctly if its outside of our in memory blockchain" in {
    chainHandler =>
      // need to generate a bunch of block headers first
      val target =
        2500 // our limit for in memory blockchains is 2016 headers currently (difficulty interval)
      val buildF = ChainUnitTest.buildNHeaders(chainHandler, target)
      val chainParams = chainHandler.chainConfig.network.chainParams
      val batchSize = 2000
      val prevStopHash = chainParams.genesisHashBE
      val startHeight = 1
      for {
        _ <- buildF
        stopBlockHeaderDb <- chainHandler.getBestBlockHeader()
        expectedStopHash <- chainHandler
          .getHeadersAtHeight(batchSize)
          .map(_.head.hashBE)
        range <- chainHandler.nextBlockHeaderBatchRange(
          prevStopHash = prevStopHash,
          stopHash = stopBlockHeaderDb.hashBE,
          batchSize = batchSize)
      } yield {
        assert(range.nonEmpty)
        assert(range.get.startHeight == startHeight)
        assert(range.get.stopBlockHashBE == expectedStopHash)
      }
  }

  it must "generate the correct range of block filters if a header is reorged" in {
    chainHandler: ChainHandler =>
      val reorgFixtureF = buildChainHandlerCompetingHeaders(chainHandler)
      val chainHandlerF = reorgFixtureF.map(_.chainApi)
      val newHeaderBF = reorgFixtureF.map(_.headerDb1)
      val newHeaderCF = reorgFixtureF.map(_.headerDb2)
      val batchSize = 100

      // two competing headers B,C built off of A
      // first specify header B to be syncing filter headers from
      val assert0F = for {
        chainHandler <- chainHandlerF
        newHeaderB <- newHeaderBF
        newHeaderC <- newHeaderCF
        blockHeaderBatchOpt <- chainHandler.nextFilterHeaderBatchRange(
          stopBlockHash = newHeaderB.hashBE,
          batchSize = batchSize)
      } yield {
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        ChainHandlerTest.checkReorgHeaders(header1 = newHeaderB,
                                           header2 = newHeaderC,
                                           bestHash = marker.stopBlockHash.flip)
        assert(marker.startHeight == 0)
        assert(marker.stopBlockHash.flip == newHeaderB.hashBE)
      }

      // two competing headers B,C built off of A
      // first specify header C to be syncing filter headers from
      val assert1F = for {
        _ <- assert0F
        chainHandler <- chainHandlerF
        newHeaderB <- newHeaderBF
        newHeaderC <- newHeaderCF
        blockHeaderBatchOpt <- chainHandler.nextFilterHeaderBatchRange(
          stopBlockHash = newHeaderC.hashBE,
          batchSize = batchSize)
      } yield {
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        ChainHandlerTest.checkReorgHeaders(header1 = newHeaderB,
                                           header2 = newHeaderC,
                                           bestHash = marker.stopBlockHash.flip)
        assert(marker.startHeight == 0)
        assert(marker.stopBlockHash.flip == newHeaderC.hashBE)
      }

      // now let's build a new block header ontop of C and process it
      // when we call chainHandler.nextFilterHeaderBatchRange it
      // should be C's hash instead of B's hash
      for {
        _ <- assert1F
        chainHandler <- chainHandlerF
        headerC <- newHeaderCF
        headerD = BlockHeaderHelper.buildNextHeader(headerC)
        chainApiD <- chainHandler.processHeader(headerD.blockHeader)
        blockHeaderBatchOpt <- chainApiD.nextFilterHeaderBatchRange(
          stopBlockHash = headerD.hashBE,
          batchSize = batchSize)
        count <- chainApiD.getBlockCount()
      } yield {
        assert(count == 2)
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        assert(marker.startHeight == 0)
        assert(headerD.hash == marker.stopBlockHash)
      }

  }

  it must "generate a range for a block filter query for the genesis block" in {
    chainHandler: ChainHandler =>
      val genesisHeader =
        chainHandler.chainConfig.chain.genesisBlock.blockHeader
      val assert1F = for {
        rangeOpt <-
          chainHandler.nextFilterHeaderBatchRange(stopBlockHash =
                                                    genesisHeader.hashBE,
                                                  1)
      } yield {
        assert(rangeOpt.nonEmpty)
        val marker = rangeOpt.get
        assert(marker.startHeight == 0)
        assert(marker.stopBlockHash == genesisHeader.hash)
      }

      // let's process a block header, and then be able to fetch that header as the last stopHash
      val blockHeaderDb = {
        BlockHeaderDbHelper.fromBlockHeader(height = 0,
                                            chainWork =
                                              Pow.getBlockProof(genesisHeader),
                                            bh = genesisHeader)
      }

      val blockHeader = BlockHeaderHelper.buildNextHeader(blockHeaderDb)
      val chainApi2 = assert1F.flatMap { _ =>
        chainHandler.processHeader(blockHeader.blockHeader)
      }

      for {
        chainApi <- chainApi2
        rangeOpt <-
          chainApi.nextFilterHeaderBatchRange(stopBlockHash =
                                                blockHeader.hashBE,
                                              batchSize = 2)
      } yield {
        val marker = rangeOpt.get
        assert(rangeOpt.nonEmpty)
        assert(marker.startHeight == 0)
        assert(marker.stopBlockHash == blockHeader.hash)
      }
  }

  it must "nextFilterHeaderBatchRange must honor the batchSize query" in {
    chainHandler: ChainHandler =>
      val reorgFixtureF = buildChainHandlerCompetingHeaders(chainHandler)
      val chainHandlerF = reorgFixtureF.map(_.chainApi)
      val newHeaderBF = reorgFixtureF.map(_.headerDb1)
      val newHeaderCF = reorgFixtureF.map(_.headerDb2)

      // two competing headers B,C built off of A
      // first specify header C to be syncing filter headers from
      val assert1F = for {
        chainHandler <- chainHandlerF
        newHeaderB <- newHeaderBF
        newHeaderC <- newHeaderCF
        blockHeaderBatchOpt <- chainHandler.nextFilterHeaderBatchRange(
          stopBlockHash = newHeaderC.hashBE,
          batchSize = 2)
      } yield {
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        ChainHandlerTest.checkReorgHeaders(header1 = newHeaderB,
                                           header2 = newHeaderC,
                                           bestHash = marker.stopBlockHash.flip)
        assert(marker.startHeight == 0)
        assert(newHeaderC.hashBE == marker.stopBlockHash.flip)
      }

      val headerDF = {
        newHeaderCF.map(headerC => BlockHeaderHelper.buildNextHeader(headerC))
      }
      // now let's build a new block header ontop of C and process it
      // when we call chainHandler.nextFilterHeaderBatchRange with batchSize=3
      // should get D's hash back as the stop hash
      val assert3F = for {
        _ <- assert1F
        chainHandler <- chainHandlerF
        headerD <- headerDF
        chainApiD <- chainHandler.processHeader(headerD.blockHeader)
        blockHeaderBatchOpt <- chainApiD.nextFilterHeaderBatchRange(
          stopBlockHash = headerD.hashBE,
          batchSize = 3)
      } yield {
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        assert(marker.startHeight == 0)
        assert(marker.stopBlockHash.flip == headerD.hashBE)
      }

      assert3F
  }

  it must "nextFilterHeaderBatchRange must honor the startHeightOpt parameter" in {
    chainHandler =>
      val reorgFixtureF = buildChainHandlerCompetingHeaders(chainHandler)
      val chainHandlerF = reorgFixtureF.map(_.chainApi)
      val newHeaderCF = reorgFixtureF.map(_.headerDb2)
      val assert1F = for {
        chainHandler <- chainHandlerF
        newHeaderC <- newHeaderCF
        rangeOpt <- chainHandler.nextFilterHeaderBatchRange(stopBlockHash =
                                                              newHeaderC.hashBE,
                                                            batchSize = 2,
                                                            startHeightOpt =
                                                              Some(0))
      } yield {
        assert(rangeOpt.nonEmpty, s"rangeOpt=$rangeOpt")
        val range = rangeOpt.get
        assert(range.startHeight == 0)
        assert(range.stopBlockHashBE == newHeaderC.hashBE)
      }
      assert1F
  }

  it must "generate the next range of filters correctly if its outside of our in memory blockchain" in {
    chainHandler =>
      // need to generate a bunch of block headers first
      val target =
        2500 // our limit for in memory blockchains is 2016 headers currently (difficulty interval)
      val buildF = ChainUnitTest.buildNHeaders(chainHandler, target)
      val batchSize = 2000
      val startHeight = 0
      for {
        _ <- buildF
        stopBlockHeaderDb <- chainHandler.getBestBlockHeader()
        expectedStopHash <- chainHandler
          .getHeadersAtHeight(batchSize - 1)
          .map(_.head.hashBE)
        range <- chainHandler.nextFilterHeaderBatchRange(
          stopBlockHash = stopBlockHeaderDb.hashBE,
          batchSize = batchSize)
      } yield {
        assert(range.nonEmpty)
        assert(range.get.startHeight == startHeight)
        assert(range.get.stopBlockHashBE == expectedStopHash)
      }
  }

  it must "read compact filters for the database" in {
    chainHandler: ChainHandler =>
      for {
        _ <- insertGenesisFilterHeaderAndFilter(chainHandler)
        bestBlock <- chainHandler.getBestBlockHeader()
        filterHeader <- chainHandler.getFilterHeader(bestBlock.hashBE)
        filter <- chainHandler.getFilter(bestBlock.hashBE)
      } yield {
        assert(filterHeader.isDefined)
        assert(filter.isDefined)
        assert(filterHeader.get.filterHashBE == filter.get.hashBE)
      }
  }

  it must "return the number of confirmations for the chain tip" in {
    chainHandler: ChainHandler =>
      for {
        bestBlockHashBE <- chainHandler.getBestBlockHash()
        confirmations <- chainHandler.getNumberOfConfirmations(bestBlockHashBE)
      } yield {
        assert(confirmations.contains(1))
      }
  }

  it must "return the number of confirmations" in {
    chainHandler: ChainHandler =>
      val headers = 0.until(20).foldLeft(Vector(genesis)) { (accum, _) =>
        accum :+ BlockHeaderHelper.buildNextHeader(accum.last)
      }

      for {
        _ <- chainHandler.blockHeaderDAO.createAll(headers.tail)

        confirmations <-
          chainHandler.getNumberOfConfirmations(headers(3).hashBE)
      } yield {
        assert(confirmations.contains(18))
      }
  }

  it must "return none for the number of confirmations for a non-existent block" in {
    chainHandler: ChainHandler =>
      chainHandler.getNumberOfConfirmations(DoubleSha256DigestBE.empty).map {
        result =>
          assert(result.isEmpty)
      }
  }

  // G -> A -> B
  // G -> C -> D -> E
  it must "return none for the number of confirmations for a reorged block" in {
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
        confs <- reorged.getNumberOfConfirmations(oldSecond.hashBE)
      } yield assert(confs.isEmpty)
  }

  it must "return the height by block stamp" in { chainHandler: ChainHandler =>
    for {
      bestBlock <- chainHandler.getBestBlockHeader()
      stamp1 = BlockStamp.BlockHash(bestBlock.hashBE)
      stamp2 = BlockStamp.BlockHeight(bestBlock.height)
      height1 <- chainHandler.getHeightByBlockStamp(stamp1)
      height2 <- chainHandler.getHeightByBlockStamp(stamp2)
    } yield {
      assert(height1 == height2)
    }
  }

  it must "fail to return the height by block time" in {
    chainHandler: ChainHandler =>
      recoverToSucceededIf[RuntimeException] {
        for {
          bestBlock <- chainHandler.getBestBlockHeader()
          stamp = BlockStamp.BlockTime(bestBlock.time)
          height <- chainHandler.getHeightByBlockStamp(stamp)
        } yield height
      }
  }

  it must "fail to return the height by block stamp with an unknown hash" in {
    chainHandler: ChainHandler =>
      recoverToSucceededIf[UnknownBlockHash] {
        val stamp = BlockStamp.BlockHash(DoubleSha256DigestBE.empty)
        chainHandler.getHeightByBlockStamp(stamp)
      }
  }

  it must "find filters between heights" in { chainHandler: ChainHandler =>
    for {
      _ <- insertGenesisFilterHeaderAndFilter(chainHandler)
      filters <- chainHandler.getFiltersBetweenHeights(0, 1)
    } yield {
      val genesis = ChainTestUtil.genesisFilterDb
      val genesisFilterResponse = FilterResponse(genesis.golombFilter,
                                                 genesis.blockHashBE,
                                                 genesis.height)

      assert(filters == Vector(genesisFilterResponse))
    }
  }

  it must "return filters in order by block height" in { chainHandler =>
    val maxHeightF = chainHandler.getBestHashBlockHeight()
    for {
      maxHeight <- maxHeightF
      filters <- chainHandler.getFiltersBetweenHeights(0, maxHeight)
    } yield {
      assert(filters.sortBy(_.blockHeight) == filters)
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

  it must "not throw an exception when processing a filter we have already  seen" in {
    chainHandler: ChainHandler =>
      val filter = ChainTestUtil.genesisFilterMessage
      val filters = Vector.fill(2)(filter)
      for {
        _ <- insertGenesisFilterHeaderAndFilter(chainHandler)
        beforeFilterCount <- chainHandler.getFilterCount()
        _ <- chainHandler.processFilters(filters)
        filterCount <- chainHandler.getFilterCount()
      } yield {
        assert(beforeFilterCount == filterCount)
      }
  }

  it must "process no filters" in { chainHandler: ChainHandler =>
    chainHandler.processFilters(Vector.empty).map { newHandler =>
      assert(chainHandler == newHandler)
    }
  }

  it must "check isMissingChainWork when there is over a 100 headers" in {
    chainHandler: ChainHandler =>
      val genesis = ChainTestUtil.genesisHeaderDb
      val headers = 0.to(101).foldLeft(Vector(genesis)) { (accum, _) =>
        val next = BlockHeaderHelper.buildNextHeader(accum.last)
        accum :+ next
      }

      val noWork = BlockHeaderHelper
        .buildNextHeader(headers.last)
        .copy(chainWork = BigInt(0))

      for {
        _ <- chainHandler.blockHeaderDAO.upsertAll(headers)
        isMissingFirst100 <- chainHandler.isMissingChainWork
        _ = assert(!isMissingFirst100)
        _ <- chainHandler.blockHeaderDAO.create(noWork)
        isMissingLast100 <- chainHandler.isMissingChainWork
      } yield assert(isMissingLast100)
  }

  it must "get best filter" in { chainHandler: ChainHandler =>
    for {
      _ <- insertGenesisFilterHeaderAndFilter(chainHandler)
      bestFilterHeaderOpt <- chainHandler.getBestFilterHeader()
      bestFilterOpt <- chainHandler.getBestFilter()
    } yield {
      assert(bestFilterHeaderOpt.isDefined)
      assert(bestFilterOpt.isDefined)
      assert(bestFilterOpt.get.hashBE == bestFilterHeaderOpt.get.filterHashBE)
    }
  }

  it must "execute sync callback" in { chainHandler: ChainHandler =>
    @volatile var values = Vector.empty[Boolean]
    val callback: OnSyncFlagChanged = { (value: Boolean) =>
      Future {
        synchronized {
          values = values :+ value
        }
      }
    }

    val callbacks = ChainCallbacks.onOnSyncFlagChanged(callback)
    chainHandler.chainConfig.addCallbacks(callbacks)

    for {
      _ <- chainHandler.setSyncing(false)
      _ <- chainHandler.setSyncing(false)
      _ <- chainHandler.setSyncing(true)
      _ <- chainHandler.setSyncing(true)
      _ <- chainHandler.setSyncing(false)
      _ <- chainHandler.setSyncing(false)
      _ <- AsyncUtil.awaitCondition { () => synchronized { values.size == 2 } }
    } yield {
      assert(values == Vector(true, false))
    }

  }
}

object ChainHandlerTest {

  /** Checks that
    *   1. The header1 & header2 have the same chainwork 2. Checks that header1
    *      and header2 have the same time 3. Checks bestHash is one of
    *      header1.hashBE or header2.hashBE
    */
  def checkReorgHeaders(
      header1: BlockHeaderDb,
      header2: BlockHeaderDb,
      bestHash: DoubleSha256DigestBE): Assertion = {
    Assertions.assert(header1.chainWork == header2.chainWork)
    Assertions.assert(header1.time == header2.time)
    // if both chainwork and time are the same, we are left to
    // how the database serves up the data
    // just make sure it is one of the two headers
    Assertions.assert(Vector(header1.hashBE, header2.hashBE).contains(bestHash))
  }
}
