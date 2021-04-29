package org.bitcoins.chain.blockchain

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.testkit.chain.ChainDbUnitTest
import org.bitcoins.testkit.chain.fixture.BitcoindChainHandlerViaZmq
import org.scalatest.FutureOutcome

import scala.concurrent.duration.DurationInt

class BitcoindChainHandlerViaZmqTest extends ChainDbUnitTest {

  override type FixtureParam = BitcoindChainHandlerViaZmq

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withBitcoindChainHandlerViaZmq(test)

  behavior of "BitcoindChainHandlerViaZmq"

  it must "peer with bitcoind via zmq and have blockchain info relayed" in {
    bitcoindChainHandler: BitcoindChainHandlerViaZmq =>
      val bitcoind = bitcoindChainHandler.bitcoindRpc

      val chainHandler = bitcoindChainHandler.chainHandler

      val bitcoindBlockCountF = bitcoind.getBlockCount
      for {
        bitcoinSBlockCount <-
          chainHandler
            .getBlockCount()
        bitcoindCount <- bitcoindBlockCountF
        _ = assert(bitcoindCount == bitcoinSBlockCount)
        address <- bitcoind.getNewAddress
        hash +: _ <- bitcoind.generateToAddress(1, address)
        _ <- {
          //test case is totally async since we
          //can't monitor processing flow for zmq
          //so we just need to await until we
          //have fully processed the header
          AsyncUtil.awaitConditionF(
            () => chainHandler.getHeader(hash).map(_.isDefined),
            interval = 250.millis)
        }
        header <- chainHandler.getHeader(hash)
      } yield assert(header.get.hashBE == hash)
  }

}
