package org.bitcoins.chain.blockchain.sync

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.chain.ChainUnitTest
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
      val getBestBlockHashFunc = { () =>
        bitcoind.getBestBlockHash
      }

      val getBlockHeaderFunc = { hash: DoubleSha256DigestBE =>
        bitcoind.getBlockHeader(hash).map(_.blockHeader)
      }

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
}
