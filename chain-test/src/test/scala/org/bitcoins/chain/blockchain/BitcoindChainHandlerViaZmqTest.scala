package org.bitcoins.chain.blockchain

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.chain.{ChainDbUnitTest, ChainUnitTest}
import org.bitcoins.testkit.chain.fixture.BitcoindChainHandlerViaZmq
import org.scalatest.{Assertion, FutureOutcome}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Random

class BitcoindChainHandlerViaZmqTest extends ChainDbUnitTest {

  override type FixtureParam = BitcoindChainHandlerViaZmq

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withBitcoindChainHandlerViaZmq(test)(using system, chainAppConfig)

  behavior of "BitcoindChainHandlerViaZmq"

  it must "peer with bitcoind via zmq and have blockchain info relayed" in {
    (bitcoindChainHandler: BitcoindChainHandlerViaZmq) =>
      val bitcoind = bitcoindChainHandler.bitcoindRpc

      val chainHandler = bitcoindChainHandler.chainHandler

      val bitcoindBlockCountF = bitcoind.getBlockCount()
      for {
        bitcoinSBlockCount <-
          chainHandler
            .getBlockCount()
        bitcoindCount <- bitcoindBlockCountF
        _ = assert(bitcoindCount == bitcoinSBlockCount)
        address <- bitcoind.getNewAddress
        case hash +: _ <- bitcoind.generateToAddress(1, address)
        _ <- {
          // test case is totally async since we
          // can't monitor processing flow for zmq
          // so we just need to await until we
          // have fully processed the header
          AsyncUtil.awaitConditionF(
            () => chainHandler.getHeader(hash).map(_.isDefined),
            interval = 250.millis
          )
        }
        header <- chainHandler.getHeader(hash)
      } yield assert(header.get.hashBE == hash)
  }

  it must "correctly calculate the mediantimepast the same as bitcoind" in {
    (bitcoindChainHandler: BitcoindChainHandlerViaZmq) =>
      val bitcoind = bitcoindChainHandler.bitcoindRpc

      val chainHandler = bitcoindChainHandler.chainHandler
      for {
        blockCount <- bitcoind.getBlockCount()
        _ <- ChainUnitTest.isSynced(chainHandler, bitcoind)
        numBlocks = Random.nextInt(100)
        _ <- genNBlocksCheckMTP(bitcoind, chainHandler, numBlocks)
        newBlockCount <- chainHandler.getBlockCount()
      } yield {
        assert(blockCount + numBlocks == newBlockCount)
      }
  }

  def genNBlocksCheckMTP(
      bitcoind: BitcoindRpcClient,
      chainHandler: ChainApi,
      n: Int): Future[Assertion] = {
    for {
      _ <- bitcoind.generate(n)
      _ <- AsyncUtil.awaitConditionF(() =>
        ChainUnitTest.isSynced(chainHandler, bitcoind))
      bitcoindMTP <- bitcoind.getMedianTimePast()
      bitcoinSMTP <- chainHandler.getMedianTimePast()
    } yield assert(bitcoinSMTP == bitcoindMTP)
  }
}
