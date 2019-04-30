package org.bitcoins.chain.blockchain.sync

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.util.{BitcoindChainHandlerViaRpc, ChainUnitTest}
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.scalatest.FutureOutcome

import scala.concurrent.Future

class ChainSyncTest extends ChainUnitTest {
  override type FixtureParam = BitcoindChainHandlerViaRpc

  override implicit val system = ActorSystem(s"chain-sync-test-${System.currentTimeMillis()}")

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
        val block1F = bitcoind.generate(1)
        val newChainHandlerF: Future[ChainApi] = block1F.flatMap { hashes =>
          ChainSync.sync(chainHandler = chainHandler,
            getBlockHeaderFunc = getBlockHeaderFunc,
            getBestBlockHashFunc = getBestBlockHashFunc)
        }

      newChainHandlerF.flatMap { chainHandler =>

        chainHandler.getBlockCount.map(count => assert(count == 1))

      }
  }
}
