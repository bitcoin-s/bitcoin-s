package org.bitcoins.core.api

import org.bitcoins.core.util.FutureUtil
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalatest.Assertion

import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.util.Success

class CallbackTest extends BitcoinSAsyncTest {

  val testTimeout: FiniteDuration = 10.seconds

  it must "show callbacks being blocked" in {
    val promise = Promise[Assertion]()

    val f1: Callback[Unit] = _ => {
      Thread.sleep(testTimeout.toMillis)
      promise.complete(fail("2nd callback did not start before timeout"))
      FutureUtil.unit
    }
    val f2: Callback[Unit] = _ => {
      promise.complete(Success(succeed))
      FutureUtil.unit
    }
    val handler =
      CallbackHandler[Unit, Callback[Unit]](name = "name", Vector(f1, f2))

    // Start execution of callbacks
    handler.execute(())

    // Return result of the callbacks, f2 should complete first
    promise.future
  }
}
