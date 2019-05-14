package org.bitcoins.chain.blockchain

import akka.actor.ActorSystem
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.chain.fixture.BitcoindChainHandlerViaZmq
import org.scalatest.FutureOutcome

import scala.concurrent.Future

class BitcoindChainHandlerViaZmqTest extends ChainUnitTest {

  override type FixtureParam = BitcoindChainHandlerViaZmq

  override implicit val system: ActorSystem = ActorSystem("ChainUnitTest")

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withBitcoindChainHandlerViaZmq(test)

  behavior of "BitcoindChainHandlerViaZmq"

  it must "peer with bitcoind via zmq and have blockchain info relayed" in {
    bitcoindChainHandler: BitcoindChainHandlerViaZmq =>
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
