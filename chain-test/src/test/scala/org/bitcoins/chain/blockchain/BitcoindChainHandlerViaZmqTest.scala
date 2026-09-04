package org.bitcoins.chain.blockchain

import org.apache.pekko.stream.{OverflowStrategy, QueueOfferResult}
import org.apache.pekko.stream.scaladsl.{Sink, Source}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.chain.fixture.BitcoindChainHandlerViaZmq
import org.scalatest.{Assertion, FutureOutcome}

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.util.Random

class BitcoindChainHandlerViaZmqTest extends ChainUnitTest {

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
        numBlocks = Random.between(5, 100)
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

  // The two tests below reproduce and validate the fix for the queue race in
  // ChainUnitTest.createChainHandlerWithBitcoindZmq's handleRawBlock: it used
  // to call queue.offer(block) without waiting for the previously returned
  // Future to complete. ZMQSubscriber's receive loop invokes that listener
  // synchronously and moves on to the next message as soon as the call
  // returns, so a burst of zmq messages (e.g. from generating many blocks in
  // one bitcoind RPC call, as "correctly calculate the mediantimepast" above
  // does) fired concurrent, unawaited offer() calls against one Source.queue.
  // That's unsafe -- the calls race each other and the queue can stall --
  // which was silently dropping blocks and produced flakes like "not enough
  // blocks in the chain, got only N headers". These don't need
  // bitcoind/zmq -- they isolate the Source.queue usage pattern directly.

  it must "drop elements when Source.queue offer is not awaited between calls" in {
    (_: BitcoindChainHandlerViaZmq) =>
      val processed = new AtomicInteger(0)

      // slow downstream, same shape as the real processHeaders() call in
      // ChainUnitTest -- takes long enough that a tight offer loop will race it
      val sink = Sink.foreachAsync[Int](1) { (_: Int) =>
        Future {
          Thread.sleep(20)
          processed.incrementAndGet()
          ()
        }(using executionContext)
      }

      val queue = Source
        .queue[Int](10, OverflowStrategy.backpressure)
        .to(sink)
        .run()

      val n = 100

      // the pre-fix bug: fire offer() for every element back-to-back without
      // waiting for the previous call's returned Future to complete
      (0 until n).foreach { i =>
        queue
          .offer(i)
          .foreach {
            case QueueOfferResult.Enqueued    => ()
            case QueueOfferResult.Dropped     => ()
            case QueueOfferResult.Failure(_)  => ()
            case QueueOfferResult.QueueClosed => ()
          }(using executionContext)
      }

      // bounded wait rather than "retry until all n accounted for" -- the
      // race can wedge the queue so that some offers never resolve at all
      org.apache.pekko.pattern
        .after(3.seconds)(Future.unit)(using system)
        .map { _ =>
          // never all n make it through the sink within the wait window
          assert(processed.get() < n)
        }
  }

  it must "process all elements when each Source.queue offer is awaited" in {
    (_: BitcoindChainHandlerViaZmq) =>
      val processed = new AtomicInteger(0)

      val sink = Sink.foreachAsync[Int](1) { (_: Int) =>
        Future {
          Thread.sleep(20)
          processed.incrementAndGet()
          ()
        }(using executionContext)
      }

      val queue = Source
        .queue[Int](10, OverflowStrategy.backpressure)
        .to(sink)
        .run()

      val n = 100

      // the fix applied in ChainUnitTest.createChainHandlerWithBitcoindZmq:
      // block on each offer's Future before issuing the next one, so calls
      // never race each other
      (0 until n).foreach { i =>
        Await.result(queue.offer(i), 5.seconds)
        ()
      }

      AsyncUtil
        .awaitCondition(() => processed.get() == n,
                        interval = 50.millis,
                        maxTries = 100)(using executionContext)
        .map(_ => assert(processed.get() == n))
  }
}
