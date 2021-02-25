package org.bitcoins.core.api

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.testkitcore.util.BitcoinSJvmTest
import org.scalatest.Assertion

import java.util.concurrent.TimeUnit
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.util.Success

class CallbackTest extends BitcoinSJvmTest {

  val testTimeout: FiniteDuration = 10.seconds

  it must "show callbacks being blocked" in {
    val promise = Promise[Assertion]()

    val f1: Callback[Unit] = _ => {

      val runnable: Runnable = () => {
        if (!promise.isCompleted) {
          promise.failure(
            new RuntimeException("2nd callback did not start before timeout"))
        }
      }
      AsyncUtil.scheduler.schedule(runnable,
                                   testTimeout.toMillis,
                                   TimeUnit.MILLISECONDS)
      promise.future.map(_ => ())
    }

    val f2: Callback[Unit] = _ => {
      if (!promise.isCompleted) {
        promise.complete(Success(succeed))
      }
      promise.future.map(_ => ())
    }

    val handler =
      CallbackHandler[Unit, Callback[Unit]](name = "name", Vector(f1, f2))

    // Start execution of callbacks
    handler.execute(())

    // Return result of the callbacks, f2 should complete first
    promise.future
  }
}
