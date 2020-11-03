package org.bitcoins.chain.blockchain

import org.bitcoins.chain.{ChainCallbacks, OnBlockHeaderConnected}
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.chain.db.{BlockHeaderDb, BlockHeaderDbHelper}
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
import org.bitcoins.testkit.util.FileUtil
import org.scalatest.FutureOutcome
import play.api.libs.json.Json

import scala.concurrent.{Future, Promise}

class ChainHandlerTest extends ChainDbUnitTest {

  override type FixtureParam = ChainHandler

  override val defaultTag: ChainFixtureTag =
    ChainFixtureTag.GenesisChainHandlerWithFilter

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainHandlerGenesisFilter(test)

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

  it must "throw an error when we have no chains" in {
    chainHandler: ChainHandler =>
      val handler = chainHandler.copy(blockchains = Vector.empty)

      recoverToSucceededIf[RuntimeException] {
        handler.getBestBlockHeader()
      }
  }

  it must "throw an error when we have no headers" in {
    chainHandler: ChainHandler =>
      val handler =
        chainHandler.copy(blockchains = Vector(Blockchain(Vector.empty)))

      recoverToSucceededIf[RuntimeException] {
        handler.getBestBlockHeader()
      }
  }

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
    val arrStr = source.getLines().next()
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

  it must "not fail ChainHandler.processHeaders() with empty headers collection" in {
    chainHandler: ChainHandler =>
      for {
        _ <- chainHandler.processHeaders(Vector.empty)
      } yield succeed
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
        bestHash <- chainHandler.getBestBlockHash()
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
        bestHash <- chainHandlerF.getBestBlockHash()
      } yield assert(bestHash == headerE.hashBE)
  }

  it must "get the highest filter header" in { chainHandler: ChainHandler =>
    {
      for {
        count <- chainHandler.getFilterHeaderCount()
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
        count <- chainHandler.getFilterCount()
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

  it must "generate a range for a block filter query for the genesis block" in {
    chainHandler: ChainHandler =>
      val genesisHeader =
        chainHandler.chainConfig.chain.genesisBlock.blockHeader
      val assert1F = for {
        rangeOpt <-
          chainHandler.nextBlockHeaderBatchRange(DoubleSha256DigestBE.empty, 1)
      } yield {
        val marker = rangeOpt.get
        assert(rangeOpt.nonEmpty)
        assert(marker.startHeight == 0)
        assert(marker.stopBlockHash == genesisHeader.hash)
      }

      //let's process a block header, and then be able to fetch that header as the last stopHash
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
          chainApi.nextBlockHeaderBatchRange(DoubleSha256DigestBE.empty, 2)
      } yield {
        val marker = rangeOpt.get
        assert(rangeOpt.nonEmpty)
        assert(marker.startHeight == 0)
        assert(marker.stopBlockHash == blockHeader.hash)
      }
  }

  it must "generate the correct range of block filters if a header is reorged" in {
    chainHandler: ChainHandler =>
      val reorgFixtureF = buildChainHandlerCompetingHeaders(chainHandler)
      val chainHandlerF = reorgFixtureF.map(_.chainApi)
      val newHeaderBF = reorgFixtureF.map(_.headerDb1)
      val newHeaderCF = reorgFixtureF.map(_.headerDb2)
      val batchSize = 100

      //two competing headers B,C built off of A
      //so just pick the first headerB to be our next block header batch
      val assert1F = for {
        chainHandler <- chainHandlerF
        newHeaderB <- newHeaderBF
        blockHeaderBatchOpt <- chainHandler.nextBlockHeaderBatchRange(
          prevStopHash = ChainTestUtil.regTestGenesisHeaderDb.hashBE,
          batchSize = batchSize)
      } yield {
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        assert(newHeaderB.hash == marker.stopBlockHash)
        assert(newHeaderB.height == marker.startHeight)
      }

      //now let's build a new block header ontop of C and process it
      //when we call chainHandler.nextBlockHeaderBatchRange it
      //should be C's hash instead of B's hash
      for {
        _ <- assert1F
        chainHandler <- chainHandlerF
        headerC <- newHeaderCF
        headerD = BlockHeaderHelper.buildNextHeader(headerC)
        chainApiD <- chainHandler.processHeader(headerD.blockHeader)
        blockHeaderBatchOpt <- chainApiD.nextBlockHeaderBatchRange(
          prevStopHash = ChainTestUtil.regTestGenesisHeaderDb.hashBE,
          batchSize = batchSize)
        count <- chainApiD.getBlockCount()
      } yield {
        assert(count == 2)
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        assert(headerC.height == marker.startHeight)
        assert(headerD.hash == marker.stopBlockHash)
      }

  }

  it must "return None for ChainHandler.nextBlockHeaderBatchRange if we are synced" in {
    chainHandler: ChainHandler =>
      val genesisHeader =
        chainHandler.chainConfig.chain.genesisBlock.blockHeader
      val assert1F = for {
        rangeOpt <-
          chainHandler.nextBlockHeaderBatchRange(genesisHeader.hashBE, 1)
      } yield {
        assert(rangeOpt.isEmpty)
      }
      assert1F
  }

  it must "generate a range for a block filter header query" in {
    chainHandler: ChainHandler =>
      for {
        bestBlock <- chainHandler.getBestBlockHeader()
        bestBlockHashBE = bestBlock.hashBE
        rangeOpt <-
          chainHandler.nextFilterHeaderBatchRange(DoubleSha256DigestBE.empty, 1)
      } yield {
        val marker = rangeOpt.get
        assert(rangeOpt.nonEmpty)
        assert(marker.startHeight == 0)
        assert(marker.stopBlockHash == bestBlockHashBE.flip)
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
    chainHandler.getFiltersBetweenHeights(0, 1).map { filters =>
      val genesis = ChainUnitTest.genesisFilterDb
      val genesisFilterResponse = FilterResponse(genesis.golombFilter,
                                                 genesis.blockHashBE,
                                                 genesis.height)

      assert(filters == Vector(genesisFilterResponse))
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

  it must "get best filter header with zero blockchains in memory" in {
    chainHandler: ChainHandler =>
      val noChainsChainHandler = chainHandler.copy(blockchains = Vector.empty)

      for {
        filterHeaderOpt <- noChainsChainHandler.getBestFilterHeader()
      } yield {
        assert(filterHeaderOpt.isDefined)
        assert(filterHeaderOpt.get == ChainUnitTest.genesisFilterHeaderDb)
      }
  }

  it must "fail when processing duplicate filters" in {
    chainHandler: ChainHandler =>
      recoverToSucceededIf[DuplicateFilters] {
        val filters = Vector.fill(2)(ChainUnitTest.genesisFilterMessage)

        chainHandler.processFilters(filters)
      }
  }

  it must "process no filters" in { chainHandler: ChainHandler =>
    chainHandler.processFilters(Vector.empty).map { newHandler =>
      assert(chainHandler == newHandler)
    }
  }

  it must "check isMissingChainWork when there is over a 100 headers" in {
    chainHandler: ChainHandler =>
      val genesis = ChainUnitTest.genesisHeaderDb
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

  it must "process a new valid block header with a callback" in {
    chainHandler: ChainHandler =>
      val resultP: Promise[Boolean] = Promise()

      val callback: OnBlockHeaderConnected = (_: Int, _: BlockHeader) => {
        Future {
          resultP.success(true)
          ()
        }
      }

      val callbacks = ChainCallbacks.onBlockHeaderConnected(callback)
      chainHandler.chainConfig.addCallbacks(callbacks)

      val newValidHeader =
        BlockHeaderHelper.buildNextHeader(ChainUnitTest.genesisHeaderDb)

      for {
        _ <- chainHandler.processHeader(newValidHeader.blockHeader)
        result <- resultP.future
      } yield assert(result)
  }
}
