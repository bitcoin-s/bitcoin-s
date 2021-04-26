package org.bitcoins.chain.blockchain.sync

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.chain.SyncUtil
import org.bitcoins.testkit.chain.fixture.{
  BitcoindBaseVersionChainHandlerViaRpc,
  ChainWithBitcoindNewestCachedUnitTest
}

import scala.concurrent.Future

class ChainSyncTest extends ChainWithBitcoindNewestCachedUnitTest {

  behavior of "ChainSync"

  it must "sync our chain handler when it is not synced with bitcoind" in {
    bitcoindWithChainHandler: BitcoindBaseVersionChainHandlerViaRpc =>
      val bitcoind = bitcoindWithChainHandler.bitcoindRpc
      val chainHandler = bitcoindWithChainHandler.chainHandler
      //first we need to implement the 'getBestBlockHashFunc' and 'getBlockHeaderFunc' functions
      val getBestBlockHashFunc = SyncUtil.getBestBlockHashFunc(bitcoind)

      val getBlockHeaderFunc = SyncUtil.getBlockHeaderFunc(bitcoind)

      //let's generate a block on bitcoind
      val block1F =
        bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(1, _))
      val newChainHandlerF: Future[ChainApi] = block1F.flatMap { _ =>
        ChainSync.sync(chainHandler = chainHandler,
                       getBlockHeaderFunc = getBlockHeaderFunc,
                       getBestBlockHashFunc = getBestBlockHashFunc)
      }

      for {
        chainHandler <- newChainHandlerF
        count <- chainHandler.getBlockCount()
        bitcoindCount <- bitcoind.getBlockCount
      } yield {
        assert(bitcoindCount == count)
      }
  }

  it must "not fail when syncing a chain handler that is synced with it's external data source" in {
    bitcoindWithChainHandler: BitcoindBaseVersionChainHandlerViaRpc =>
      val bitcoind = bitcoindWithChainHandler.bitcoindRpc
      val chainHandler = bitcoindWithChainHandler.chainHandler
      //first we need to implement the 'getBestBlockHashFunc' and 'getBlockHeaderFunc' functions
      val getBestBlockHashFunc = { () =>
        bitcoind.getBestBlockHash
      }

      val getBlockHeaderFunc = { hash: DoubleSha256DigestBE =>
        bitcoind.getBlockHeader(hash).map(_.blockHeader)
      }

      //note we are not generating a block on bitcoind
      val newChainHandlerF: Future[ChainApi] =
        ChainSync.sync(chainHandler = chainHandler,
                       getBlockHeaderFunc = getBlockHeaderFunc,
                       getBestBlockHashFunc = getBestBlockHashFunc)

      val newChainHandler2F = for {
        newChainHandler <- newChainHandlerF
        //sync it again to make sure we don't fail
        newChainHandler2 <- ChainSync.sync(
          chainHandler = newChainHandler.asInstanceOf[ChainHandler],
          getBlockHeaderFunc = getBlockHeaderFunc,
          getBestBlockHashFunc = getBestBlockHashFunc)
        bitcoinSCount <- newChainHandler2.getBlockCount()
        bitcoindCount <- bitcoind.getBlockCount
      } yield assert(bitcoinSCount == bitcoindCount)

      newChainHandler2F
  }

  it must "be able to call sync() twice and not fail when nothing has happened" in {
    bitcoindWithChainHandler =>
      val bitcoind = bitcoindWithChainHandler.bitcoindRpc
      val chainHandler = bitcoindWithChainHandler.chainHandler
      //first we need to implement the 'getBestBlockHashFunc' and 'getBlockHeaderFunc' functions
      val getBestBlockHashFunc = SyncUtil.getBestBlockHashFunc(bitcoind)

      val getBlockHeaderFunc = SyncUtil.getBlockHeaderFunc(bitcoind)

      val generate1F = for {
        addr <- bitcoind.getNewAddress
        hashes <- bitcoind.generateToAddress(1, addr)
      } yield hashes

      val sync1F: Future[ChainApi] = generate1F.flatMap { _ =>
        ChainSync.sync(chainHandler = chainHandler,
                       getBlockHeaderFunc = getBlockHeaderFunc,
                       getBestBlockHashFunc = getBestBlockHashFunc)
      }

      val assertion1F = for {
        _ <- generate1F
        chainApiSync1 <- sync1F
        count <- chainApiSync1.getBlockCount()
        bestHash <- chainApiSync1.getBestBlockHash()
        bitcoindBlockCount <- bitcoind.getBlockCount
        bitcoindBestBlockHash <- bitcoind.getBestBlockHash
      } yield {
        assert(count == bitcoindBlockCount)
        assert(bestHash == bitcoindBestBlockHash)
      }

      //let's call sync again and make sure nothing bad happens
      val sync2F = for {
        _ <- assertion1F
        chainApiSync1 <- sync1F
        chainApiSync2 <-
          ChainSync.sync(chainHandler =
                           chainApiSync1.asInstanceOf[ChainHandler],
                         getBlockHeaderFunc = getBlockHeaderFunc,
                         getBestBlockHashFunc = getBestBlockHashFunc)
        count <- chainApiSync2.getBlockCount()
        _ <- generate1F
        bestHash <- chainApiSync2.getBestBlockHash()
        bitcoindBlockCount <- bitcoind.getBlockCount
        bitcoindBestBlockHash <- bitcoind.getBestBlockHash
      } yield {
        assert(count == bitcoindBlockCount)
        assert(bestHash == bitcoindBestBlockHash)
      }

      sync2F
  }
}
