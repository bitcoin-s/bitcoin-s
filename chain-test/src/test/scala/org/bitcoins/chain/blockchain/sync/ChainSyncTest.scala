package org.bitcoins.chain.blockchain.sync

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.chain.{ChainUnitTest, SyncUtil}
import org.bitcoins.testkit.chain.fixture.BitcoindChainHandlerViaRpc
import org.scalatest.FutureOutcome

import scala.concurrent.Future

class ChainSyncTest extends ChainUnitTest {
  override type FixtureParam = BitcoindChainHandlerViaRpc

  implicit override val system = ActorSystem(
    s"chain-sync-test-${System.currentTimeMillis()}")

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withBitcoindChainHandlerViaRpc(test)
  }

  behavior of "ChainSync"

  it must "sync our chain handler when it is not synced with bitcoind" in {
    bitcoindWithChainHandler: BitcoindChainHandlerViaRpc =>
      val bitcoind = bitcoindWithChainHandler.bitcoindRpc
      val chainHandler = bitcoindWithChainHandler.chainHandler
      //first we need to implement the 'getBestBlockHashFunc' and 'getBlockHeaderFunc' functions
      val getBestBlockHashFunc = SyncUtil.getBestBlockHashFunc(bitcoind)

      val getBlockHeaderFunc = SyncUtil.getBlockHeaderFunc(bitcoind)

      //let's generate a block on bitcoind
      val block1F =
        bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(1, _))
      val newChainHandlerF: Future[ChainApi] = block1F.flatMap { hashes =>
        ChainSync.sync(chainHandler = chainHandler,
                       getBlockHeaderFunc = getBlockHeaderFunc,
                       getBestBlockHashFunc = getBestBlockHashFunc)
      }

      newChainHandlerF.flatMap { chainHandler =>
        chainHandler.getBlockCount.map(count => assert(count == 1))

      }
  }

  it must "not fail when syncing a chain handler that is synced with it's external data source" in {
    bitcoindWithChainHandler: BitcoindChainHandlerViaRpc =>
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

      newChainHandlerF.flatMap { chainHandler =>
        chainHandler.getBlockCount.map(count => assert(count == 0))
      }
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
        hashes <- generate1F
        chainApiSync1 <- sync1F
        count <- chainApiSync1.getBlockCount()
        bestHash <- chainApiSync1.getBestBlockHash()
      } yield {
        assert(count == 1)
        assert(bestHash == hashes.head)
      }

      //let's call sync again and make sure nothing bad happens
      val sync2F = for {
        _ <- assertion1F
        chainApiSync1 <- sync1F
        chainApiSync2 <- ChainSync.sync(
          chainHandler = chainApiSync1.asInstanceOf[ChainHandler],
          getBlockHeaderFunc = getBlockHeaderFunc,
          getBestBlockHashFunc = getBestBlockHashFunc)
        count <- chainApiSync2.getBlockCount()
        hashes <- generate1F
        bestHash <- chainApiSync2.getBestBlockHash()
      } yield {
        assert(count == 1)
        assert(bestHash == hashes.head)
      }

      sync2F
  }
}
