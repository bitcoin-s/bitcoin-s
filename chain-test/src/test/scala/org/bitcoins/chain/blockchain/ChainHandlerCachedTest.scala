package org.bitcoins.chain.blockchain

import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.api.chain.db.BlockHeaderDbHelper
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.chain.{BlockHeaderHelper, ChainDbUnitTest}
import org.bitcoins.testkit.chain.fixture.ChainFixtureTag
import org.bitcoins.testkitcore.chain.ChainTestUtil
import org.scalatest.FutureOutcome

class ChainHandlerCachedTest extends ChainDbUnitTest {
  override type FixtureParam = ChainHandlerCached

  override val defaultTag: ChainFixtureTag =
    ChainFixtureTag.GenesisChainHandlerCachedWithFilter

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainHandlerCachedGenesisFilter(test)

  behavior of "ChainHandlerCached"

  it must "throw an error when we have no chains" in {
    chainHandlerCached: ChainHandlerCached =>
      val handler = chainHandlerCached.copy(blockchains = Vector.empty)

      recoverToSucceededIf[RuntimeException] {
        handler
          .getBestBlockHeader()
      }
  }

  it must "get best filter header with zero blockchains in memory" in {
    chainHandlerCached: ChainHandlerCached =>
      val noChainsChainHandler =
        chainHandlerCached.copy(blockchains = Vector.empty)

      for {
        filterHeaderOpt <- noChainsChainHandler.getBestFilterHeader()
      } yield {
        assert(filterHeaderOpt.isDefined)
        assert(filterHeaderOpt.get == ChainTestUtil.genesisFilterHeaderDb)
      }
  }

  it must "generate a range for a block filter query for the genesis block" in {
    chainHandler: ChainHandlerCached =>
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
        newHeaderC <- newHeaderCF
        blockHeaderBatchOpt <- chainHandler.nextBlockHeaderBatchRange(
          prevStopHash = ChainTestUtil.regTestGenesisHeaderDb.hashBE,
          batchSize = batchSize)
      } yield {
        assert(blockHeaderBatchOpt.isDefined)
        val marker = blockHeaderBatchOpt.get
        ChainHandlerTest.checkReorgHeaders(header1 = newHeaderB,
                                           header2 = newHeaderC,
                                           bestHash = marker.stopBlockHash.flip)
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
      val assert1F = for {
        bestBlockHash <- chainHandler.getBestBlockHash()
        rangeOpt <-
          chainHandler.nextBlockHeaderBatchRange(bestBlockHash, 1)
      } yield {
        assert(rangeOpt.isEmpty)
      }
      assert1F
  }
}
