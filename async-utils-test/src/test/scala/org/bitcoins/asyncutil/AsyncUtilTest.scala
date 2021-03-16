package org.bitcoins.asyncutil

import org.bitcoins.core.util.TimeUtil
import org.bitcoins.testkitcore.util.BitcoinSJvmTest

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Success

class AsyncUtilTest extends BitcoinSJvmTest {
  behavior of "AsyncUtil"

  private def trueLater(delay: Int): Future[Boolean] = {
    AsyncUtil
      .nonBlockingSleep(delay.millis)
      .map(_ => true)
  }

  private def boolLaterDoneAnd(
      bool: Boolean,
      boolFuture: Future[Boolean]): Future[Boolean] = {
    Future.successful(boolFuture.value.contains(Success(bool)))
  }

  private def boolLaterDoneAndTrue(
      trueLater: Future[Boolean]): () => Future[Boolean] = { () =>
    boolLaterDoneAnd(bool = true, trueLater)
  }

  behavior of "TestRpcUtil"

  it should "complete immediately if condition is true" in {
    AsyncUtil
      .retryUntilSatisfiedF(conditionF = () => Future.successful(true),
                            interval = 0.millis)
      .map { _ =>
        succeed
      }
  }

  it should "fail if condition is false" in {
    recoverToSucceededIf[AsyncUtil.RpcRetryException] {
      AsyncUtil.retryUntilSatisfiedF(conditionF =
                                       () => Future.successful(false),
                                     interval = 0.millis)
    }
  }

  it should "succeed after a delay" in {
    val boolLater = trueLater(delay = 250)
    AsyncUtil.retryUntilSatisfiedF(boolLaterDoneAndTrue(boolLater)).map { _ =>
      succeed
    }
  }

  it should "succeed immediately if condition is true" in {
    AsyncUtil
      .awaitCondition(condition = () => true, 0.millis)
      .map(_ => succeed)

  }

  it should "timeout if condition is false" in {
    recoverToSucceededIf[AsyncUtil.RpcRetryException] {
      AsyncUtil
        .awaitCondition(condition = () => false, interval = 0.millis)
        .map(_ => succeed)
    }
  }

  it must "retry a predicate until it is satisfied" in {
    val counter = new AtomicInteger(0)

    def incrementAndCheck: Boolean = {
      counter.incrementAndGet() == 10
    }

    AsyncUtil
      .retryUntilSatisfied(incrementAndCheck)
      .map(_ => succeed)
  }

  it must "retry a predicate that is a future until it is satisfied" in {
    val counter = new AtomicInteger(0)

    AsyncUtil
      .retryUntilSatisfiedF(() => incrementAndCheckF(counter, 10))
      .map(_ => succeed)
  }

  it must "be able to run 2 different futures in parallel" in {
    val counter = new AtomicInteger(0)
    val counter2 = new AtomicInteger(0)

    val firstF =
      AsyncUtil.retryUntilSatisfiedF(() => incrementAndCheckF(counter, 10))

    val secondF =
      AsyncUtil.retryUntilSatisfiedF(() => incrementAndCheckF(counter2, 10))

    for {
      _ <- firstF
      _ <- secondF
    } yield succeed
  }

  it must "be able to run 100 futures on the thread pool and have the all complete in a reasonable time" in {
    val numCounters = 100
    val expectedCount = 1000
    val counters = Vector.fill(numCounters)(new AtomicInteger(0))

    //try to run all these in parallel, and see if it works
    val async: Vector[Future[Unit]] = counters.map { counter =>
      AsyncUtil.retryUntilSatisfiedF(
        () => incrementAndCheckF(counter, expectedCount),
        interval = 1.millis,
        maxTries = expectedCount)
    }

    Future.sequence(async).map(_ => succeed)
  }

  it must "handle blocking tasks ok" in {
    val sleepMs = 10000
    def blockingTask(): Boolean = {
      Thread.sleep(sleepMs)
      true
    }

    //schedule a blocking task first
    val start = TimeUtil.currentEpochMs
    val _ =
      AsyncUtil.awaitCondition(blockingTask)

    //schedule a non blocking task second
    val counter1 = new AtomicInteger(0)
    val secondF =
      AsyncUtil.awaitCondition(() => incrementAndCheck(counter1, 10))

    //the second task should not be blocked to completion by the first
    for {
      _ <- secondF
    } yield {
      val stop = TimeUtil.currentEpochMs
      assert(stop - start < sleepMs)
    }
  }

  it must "handle async blocking tasks ok" in {
    val sleepMs = 10000
    def blockingTask(): Future[Boolean] = Future {
      Thread.sleep(sleepMs)
      true
    }

    //schedule a blocking task first
    val _ =
      AsyncUtil.awaitConditionF(() => blockingTask())

    //schedule a non blocking task second
    val counter1 = new AtomicInteger(0)
    val secondF =
      AsyncUtil.awaitConditionF(() => incrementAndCheckF(counter1, 10))

    //the second task should not be blocked to completion by the first
    for {
      _ <- secondF
    } yield succeed
  }

  private def incrementAndCheck(
      counter: AtomicInteger,
      expected: Int): Boolean = {
    counter.incrementAndGet() == expected
  }

  private def incrementAndCheckF(
      counter: AtomicInteger,
      expected: Int): Future[Boolean] = Future {
    incrementAndCheck(counter, expected)
  }
}
