package org.bitcoins.chain.blockchain.sync

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.testkit.chain.fixture.BitcoindV19ChainHandler
import org.bitcoins.testkit.chain.{ChainDbUnitTest, SyncUtil}
import org.scalatest.FutureOutcome

import scala.concurrent.Future

class FilterSyncTest extends ChainDbUnitTest {

  override type FixtureParam = BitcoindV19ChainHandler

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withBitcoindV19ChainHandlerViaRpc(test)
  }

  behavior of "FilterSync"

  it must "sync 1 filter header from an external data source" in { fixture =>
    val BitcoindV19ChainHandler(bitcoind, chainHandler) = fixture

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
    } yield hashes

    val syncedF = generated1BlockF.flatMap { _ =>
      syncHelper(fixture)
    }

    for {
      syncedChainApi <- syncedF
      filterHeaderCount <- syncedChainApi.getFilterHeaderCount()
      _ = assert(filterHeaderCount == 1)
      filterCount <- syncedChainApi.getFilterCount()
    } yield assert(filterCount == 1)
  }

  it must "sync a bunch of filter headers from an external data source" in {
    fixture =>
      val BitcoindV19ChainHandler(bitcoind, _) = fixture

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
        filterHeaderCount <- syncedChainApi.getFilterHeaderCount()
        _ = assert(filterHeaderCount == numBlocks)
        filterCount <- syncedChainApi.getFilterCount()
      } yield assert(filterCount == numBlocks)
  }

  it must "be able to call filterSync() and not fail when nothing has happened" in {
    fixture =>
      val BitcoindV19ChainHandler(bitcoind, _) = fixture

      val generated1BlockF = for {
        addr <- bitcoind.getNewAddress
        hashes <- bitcoind.generateToAddress(1, addr)
      } yield hashes

      val synced1F = generated1BlockF.flatMap { _ =>
        syncHelper(fixture)
      }

      val sync2F = synced1F.flatMap { chainApi =>
        syncHelper(
          fixture.copy(chainHandler = chainApi.asInstanceOf[ChainHandler]))
      }

      for {
        syncedChainApi <- sync2F
        filterHeaderCount <- syncedChainApi.getFilterHeaderCount()
        _ = assert(filterHeaderCount == 1)
        filterCount <- syncedChainApi.getFilterCount()
      } yield assert(filterCount == 1)
  }

  private def syncHelper(
      bitcoindV19ChainHandler: BitcoindV19ChainHandler): Future[ChainApi] = {
    val filterType = FilterType.Basic
    val BitcoindV19ChainHandler(bitcoind, chainHandler) =
      bitcoindV19ChainHandler
    val getBestBlockHashFunc = SyncUtil.getBestBlockHashFunc(bitcoind)
    val getBlockHeaderFunc = SyncUtil.getBlockHeaderFunc(bitcoind)

    val getFilterFunc: BlockHeader => Future[FilterWithHeaderHash] =
      SyncUtil.getFilterFunc(bitcoind, filterType)

    //first sync the chain
    val syncedHeadersF = ChainSync.sync(chainHandler = chainHandler,
                                        getBlockHeaderFunc = getBlockHeaderFunc,
                                        getBestBlockHashFunc =
                                          getBestBlockHashFunc)

    //now sync filters
    syncedHeadersF.flatMap { syncedChainHandler =>
      FilterSync.syncFilters(
        chainApi = syncedChainHandler,
        getFilterFunc = getFilterFunc
      )
    }
  }
}
