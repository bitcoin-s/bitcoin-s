package org.bitcoins.chain.blockchain.sync

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.testkit.chain.SyncUtil
import org.bitcoins.testkit.chain.fixture.{
  BitcoindBaseVersionChainHandlerViaRpc,
  ChainWithBitcoindNewestCachedUnitTest
}

import scala.concurrent.Future

class FilterSyncTest extends ChainWithBitcoindNewestCachedUnitTest {

  behavior of "FilterSync"

  it must "sync 1 filter header from an external data source" in { fixture =>
    val BitcoindBaseVersionChainHandlerViaRpc(bitcoind, chainHandler) = fixture

    val initFilterCountF = chainHandler.getFilterCount()
    val initFilterHeaderCountF = chainHandler.getFilterHeaderCount()

    val initAssertionsF = for {
      initFilterCount <- initFilterCountF
      initFilterHeaderCount <- initFilterHeaderCountF
    } yield {
      assert(initFilterCount == 0)
      assert(initFilterHeaderCount == 0)
    }

    val generated1BlockF = for {
      _ <- initAssertionsF
      addr <- bitcoind.getNewAddress
      hashes <- bitcoind.generateToAddress(1, addr)
    } yield {
      hashes
    }

    val syncedF = generated1BlockF.flatMap { _ =>
      syncHelper(fixture)
    }

    for {
      syncedChainApi <- syncedF
      filterHeaderCount <- syncedChainApi.getFilterHeaderCount()
      bitcoindFilterCount <- bitcoind.getFilterCount()
      _ = assert(filterHeaderCount == bitcoindFilterCount)
      filterCount <- syncedChainApi.getFilterCount()
    } yield assert(filterCount == bitcoindFilterCount)
  }

  it must "sync a bunch of filter headers from an external data source" in {
    fixture =>
      val BitcoindBaseVersionChainHandlerViaRpc(bitcoind, _) = fixture

      val numBlocks = 100
      val generatedBlocksF = for {
        addr <- bitcoind.getNewAddress
        hashes <- bitcoind.generateToAddress(numBlocks, addr)
      } yield hashes

      val syncedF = generatedBlocksF.flatMap { _ =>
        syncHelper(fixture)
      }

      for {
        syncedChainApi <- syncedF
        bitcoindFilterCount <- bitcoind.getFilterCount()
        filterHeaderCount <- syncedChainApi.getFilterHeaderCount()
        _ = assert(filterHeaderCount == bitcoindFilterCount)
        filterCount <- syncedChainApi.getFilterCount()
      } yield assert(filterCount == bitcoindFilterCount)
  }

  it must "be able to call filterSync() and not fail when nothing has happened" in {
    fixture =>
      val BitcoindBaseVersionChainHandlerViaRpc(bitcoind, _) = fixture

      val generated1BlockF = for {
        addr <- bitcoind.getNewAddress
        hashes <- bitcoind.generateToAddress(1, addr)
      } yield hashes

      val synced1F = generated1BlockF.flatMap { _ =>
        syncHelper(fixture)
      }

      val sync2F = synced1F.flatMap { chainApi =>
        syncHelper(
          fixture.copy(chainHandler = chainApi.asInstanceOf[ChainHandler])
        )
      }

      for {
        syncedChainApi <- sync2F
        bitcoindFilterCount <- bitcoind.getFilterCount()
        filterHeaderCount <- syncedChainApi.getFilterHeaderCount()
        _ = assert(filterHeaderCount == bitcoindFilterCount)
        filterCount <- syncedChainApi.getFilterCount()
      } yield assert(filterCount == bitcoindFilterCount)
  }

  it must "not throw an exception when calling nextFilterHeaderBatchRange for a filter header we have already seen" in {
    (bitcoindWithChainHandler) =>
      // see: https://github.com/bitcoin-s/bitcoin-s/issues/6074
      val bitcoind = bitcoindWithChainHandler.bitcoindRpc
      val synced1F = syncHelper(bitcoindWithChainHandler)
      for {
        chainHandler <- synced1F
        bestBlockHeader <- chainHandler.getBestBlockHeader()
        prevBestBlockHeader <- bitcoind.getBlockHeader(
          bestBlockHeader.previousBlockHashBE)
        _ <- chainHandler.nextFilterHeaderBatchRange(
          stopBlockHash = prevBestBlockHeader.blockHeader.hashBE,
          batchSize = 10,
          startHeightOpt = None)
      } yield {
        succeed
      }
  }

  private def syncHelper(
      bitcoindChainHandler: BitcoindBaseVersionChainHandlerViaRpc
  ): Future[ChainApi] = {
    val filterType = FilterType.Basic
    val BitcoindBaseVersionChainHandlerViaRpc(bitcoind, chainHandler) =
      bitcoindChainHandler
    val getBestBlockHashFunc = SyncUtil.getBestBlockHashFunc(bitcoind)
    val getBlockHeaderFunc = SyncUtil.getBlockHeaderFunc(bitcoind)

    val getFilterFunc: BlockHeader => Future[FilterWithHeaderHash] =
      SyncUtil.getFilterFunc(bitcoind, filterType)

    // first sync the chain
    val syncedHeadersF: Future[ChainApi] = ChainSync.sync(
      chainHandler = chainHandler,
      getBlockHeaderFunc = getBlockHeaderFunc,
      getBestBlockHashFunc = getBestBlockHashFunc
    )

    // now sync filters
    syncedHeadersF.flatMap { syncedChainHandler =>
      FilterSync.syncFilters(
        chainApi = syncedChainHandler,
        getFilterFunc = getFilterFunc
      )
    }
  }
}
