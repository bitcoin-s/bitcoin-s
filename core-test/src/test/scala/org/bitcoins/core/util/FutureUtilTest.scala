package org.bitcoins.core.util

import org.bitcoins.testkitcore.util.BitcoinSJvmTest
import org.scalatest.compatible.Assertion

import scala.concurrent._

class FutureUtilTest extends BitcoinSJvmTest with BitcoinSLogger {
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
}
