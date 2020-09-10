---
title: Syncing Blockfilters
id: filter-sync
---

The `chain` module has the ability to store [BIP157](https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki) block filters locally. Generally these filters are useful
for doing wallet rescans. The idea is you can generate a list of script pubkeys you are interested in and see if
the block filter matches the scriptPubKey.

As we demonstrated in [the chain docs](chain.md) with block headers, you can sync block filters from an external data source
as well. We are going to use bitcoind as an example of an external data source to sync filters against. It is important
that the bitcoind version you are using is >= `v19` as the [`getblockfilter`](https://github.com/bitcoin/bitcoin/blob/master/doc/release-notes/release-notes-0.19.0.1.md#new-rpcs)
rpc is implemented there. You need to make sure bitcoind is started with the `-blockfilterindex` flag. This makes it
so we can query filters.

#### Abstract idea of syncing filters.

Our internal infrastructure depends on one function to be implemented to be able to sync filters.

```scala mdoc:invisible
import org.bitcoins.core.protocol.blockchain._
import org.bitcoins.core.gcs._
import scala.concurrent.Future

import akka.actor.ActorSystem

import org.bitcoins.core.gcs._
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.chain.blockchain.sync._

import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.chain._
import org.bitcoins.testkit.chain.fixture.BitcoindV19ChainHandler

import scala.concurrent._

```

```scala mdoc:compile-only
val getFilterFunc: BlockHeader => Future[FilterWithHeaderHash] = ???
```

With `getFilterFunc` given a `BlockHeader` we can find it's associated `GolombFilter` -- which is our internal repesentation
of a BIP157 block filter.

The basic idea for `FilterSync.syncFilters()` is to look at our current best block header inside of our `ChainApi.getBestBlockHeader()`
and then check what our best block filter's block hash is with `ChainApi.getBestFilterHeader()`. If the blockfilter returned from our internal
data store is NOT associated with our best block header, we attempt to sync our filter headers to catch up to our best block header.

### Syncing block filters against bitcoind

We are going to implement `getFilterFunc` with bitcoind and then sync a few filter headers.

```scala mdoc:compile-only

implicit val system = ActorSystem(s"filter-sync-example")
implicit val ec = system.dispatcher
implicit val chainAppConfig = BitcoinSTestAppConfig.getNeutrinoTestConfig().chainConf

//let's use a helper method to get a v19 bitcoind
//instance and a chainApi
val bitcoindWithChainApiF: Future[BitcoindV19ChainHandler] = {
  ChainUnitTest.createBitcoindV19ChainHandler()
}
val bitcoindF = bitcoindWithChainApiF.map(_.bitcoind)
val chainApiF = bitcoindWithChainApiF.map(_.chainHandler)

val filterType = FilterType.Basic
val addressF = bitcoindF.flatMap(_.getNewAddress)

//this is the function that we are going to use to sync
//our internal filters against. We use this function to query
//for each block filter associated with a blockheader
val getFilterFunc: BlockHeader => Future[FilterWithHeaderHash] = { blockHeader =>
  val prevFilterResultF =
    bitcoindF.flatMap(_.getBlockFilter(blockHeader.hashBE, filterType))
  prevFilterResultF.map { filterResult =>
    FilterWithHeaderHash(filterResult.filter, filterResult.header)
  }
}

//ok enough setup, let's generate a block that we need to sync the filter for in bitcoind
val block1F = for {
  bitcoind <- bitcoindF
  address <- addressF
  hashes <- bitcoind.generateToAddress(1,address)
} yield hashes

//to be able to sync filters, we need to make sure our block headers are synced first
//so let's sync our block headers to our internal chainstate
val chainApiSyncedHeadersF = for {
  bitcoind <- bitcoindF
  handler <- chainApiF
  getBestBlockHash = SyncUtil.getBestBlockHashFunc(bitcoind)
  getBlockHeader = SyncUtil.getBlockHeaderFunc(bitcoind)
  syncedChainApiHeaders <- ChainSync.sync(handler, getBlockHeader, getBestBlockHash)
} yield syncedChainApiHeaders

//now that we have synced our 1 block header, we can now sync the 1 block filter
//associated with that header.
val chainApiSyncedFiltersF = for {
  syncedHeadersChainApi <- chainApiSyncedHeadersF
  syncedFilters <- FilterSync.syncFilters(syncedHeadersChainApi,getFilterFunc)
} yield syncedFilters

//now we should have synced our one filter, let's make sure we have it
val resultF = for {
  chainApi <- chainApiSyncedFiltersF
  filterHeaderCount <- chainApi.getFilterHeaderCount()
  filterCount <- chainApi.getFilterCount()
} yield {
  println(s"filterHeaderCount=$filterHeaderCount filterCount=$filterCount")
}

//cleanup
resultF.onComplete { _ =>
  for {
    c <- bitcoindWithChainApiF
    _ <- ChainUnitTest.destroyBitcoindV19ChainApi(c)
    _ <- system.terminate()
  } yield ()
}
```

Yay! Now we have synced block filters from an external data source. If you want to repeatedly sync you can just call

`FilterSync.syncFilters(syncedFiltersChainApi,getFilterFunc)` every time you would like to sync. Again, you need to ensure
your headers are synced before you can sync filters, so make sure that you are calling `ChainSync.sync()` before syncing
filters.