package org.bitcoins.core.util

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.testkitcore.util.BitcoinSJvmTest
import org.scalatest.compatible.Assertion

import java.time.temporal.ChronoUnit
import scala.concurrent._
import scala.concurrent.duration.DurationInt

class FutureUtilTest extends BitcoinSJvmTest {
  it must "execute futures sequentially in the correct order" in {

    val assertionP = Promise[Assertion]()
    val assertionF = assertionP.future

    val promise1 = Promise[Unit]()
    val promise2 = Promise[Unit]()

    val future1 = promise1.future
    val future2 = promise2.future

    future1.onComplete { _ =>
      if (future2.isCompleted) {
        assertionP.failure(new Error(s"future2 completed before future1"))
      }
    }

    future2.onComplete { _ =>
      if (!future1.isCompleted) {
        assertionP.failure(
          new Error(s"future1 was not complete by future2 completing"))
      } else {
        assertionP.success(succeed)
      }
    }

    val futs = FutureUtil.sequentially(List(1, 2)) {
      case 1 =>
        promise1.success(())
        Future.successful(1)
      case 2 =>
        promise2.success(())
        Future.successful(2)

    }

    futs.map(xs => assert(List(1, 2) == xs)).flatMap(_ => assertionF)
  }

  it must "execute futures in synchronous fashion with batchAndSyncExecute" in {
    val vec = 0.until(Runtime.getRuntime.availableProcessors()).toVector

    val f: Vector[Int] => Future[Vector[Int]] = { vec =>
      AsyncUtil
        .nonBlockingSleep(1.second)
        .map(_ => vec)
    }
    val start = TimeUtil.now
    val doneF =
      FutureUtil.batchAndSyncExecute(elements = vec, f = f, batchSize = 1)

    //here is how this test case works:
    //the vector above has the same number of elements as available processors in it, and the batchSize is 1
    //each function sleeps for 1000ms (1 second). If things are
    //not run in parallel, the total time should be 5 seconds (5 elements * 1 second sleep)
    for {
      _ <- doneF
      stop = TimeUtil.now
    } yield {
      val difference = ChronoUnit.MILLIS.between(start, stop)
      if (difference >= Runtime.getRuntime.availableProcessors() * 1000) {
        succeed
      } else {
        fail(
          s"Batch did not execute in parallel! difference=${difference} seconds")
      }
    }
  }

  it must "execute futures in parallel with batchAndParallelExecute" in {
    val vec = 0.until(Runtime.getRuntime.availableProcessors()).toVector

    val f: Vector[Int] => Future[Vector[Int]] = { vec =>
      AsyncUtil
        .nonBlockingSleep(1.second)
        .map(_ => vec)
    }
    val start = TimeUtil.now
    val doneF =
      FutureUtil.batchAndParallelExecute(elements = vec, f = f, batchSize = 1)

    //here is how this test case works:
    //the vector above has the same number of elements as available processors, and the batchSize is 1
    //each function sleeps for 1000ms (1 second).
    //if things are run in parallel, it should take ~1 second to run all these in parallel
    for {
      _ <- doneF
      stop = TimeUtil.now
    } yield {
      val difference = ChronoUnit.MILLIS.between(start, stop)
      if (difference < 2000) {
        succeed
      } else {
        fail(
          s"Batch did not execute in parallel! difference=${difference} seconds processors=${Runtime.getRuntime
            .availableProcessors()}")
      }
    }
  }

}
