package org.bitcoins.testkit.util

import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalatest.Assertion
import org.scalatest.exceptions.TestFailedException

import scala.concurrent.Future

class ScalaTestUtilTest extends BitcoinSUnitTest {

  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global

  behavior of "ScalaTestUtilTest"

  def t = assert(true)

  def f = assert(false)

  def futureFail =
    Future {
      //sleep for awhile and then eventually fail
      Thread.sleep(1000)
      f
    }

  it must "evaluate a Vector[Future[Assertions]] correctly" in {
    val vec1: Vector[Future[Assertion]] =
      Vector(Future.successful(t), Future.successful(t))

    ScalaTestUtil.toAssertF(vec1)

    try {
      ScalaTestUtil.toAssertF(Vector(futureFail))
    } catch {
      case _: TestFailedException => succeed
    }

  }
}
