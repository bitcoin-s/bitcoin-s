package org.bitcoins.chain.blockchain

import akka.actor.ActorSystem
import org.bitcoins.chain.util.ChainUnitTest
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.chain.BlockHeaderHelper
import org.scalatest.FutureOutcome

import scala.concurrent.Future

class ChainHandlerTest extends ChainUnitTest {

  override type FixtureParam = ChainHandler

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainHandler(test)

  behavior of "ChainHandler"

  it must "process a new valid block header, and then be able to fetch that header" in {
    chainHandler: ChainHandler =>
      val newValidHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
      val processedHeaderF =
        chainHandler.processHeader(newValidHeader.blockHeader)

      val foundHeaderF =
        processedHeaderF.flatMap(_.getHeader(newValidHeader.hashBE))

      foundHeaderF.map(found => assert(found.get == newValidHeader))
  }

  it must "peer with bitcoind via zmq and have blockchain info relayed" in withBitcoindZmqChainHandler {
    case bitcoindChainHandler: BitcoindChainHandler =>
      val bitcoind = bitcoindChainHandler.bitcoindRpc

      val chainHandler = bitcoindChainHandler.chainHandler

      val assert1F = chainHandler.getBlockCount
        .map(count => assert(count == 0))

      //mine a block on bitcoind
      val generatedF = assert1F.flatMap(_ => bitcoind.generate(1))

      generatedF.flatMap { headers =>
        val hash = headers.head
        val foundHeaderF: Future[Unit] = {
          //test case is totally async since we
          //can't monitor processing flow for zmq
          //so we just need to await until we
          //have fully processed the header
          RpcUtil.awaitConditionF(() =>
            chainHandler.getHeader(hash).map(_.isDefined))
        }

        for {
          _ <- foundHeaderF
          header <- chainHandler.getHeader(hash)
        } yield assert(header.get.hashBE == hash)
      }
  }

}
