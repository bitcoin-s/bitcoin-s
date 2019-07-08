package org.bitcoins.testkit

import util.BitcoinSUnitTest
import Implicits._
import org.scalatest.exceptions.TestFailedException
import ammonite.terminal.LazyList

class ImplicitsTest extends BitcoinSUnitTest {

  behavior of "AssertionSeqOps"

  it should "flatten succeeded assertions" in {
    val assertions = List(succeed, assert(true), assert(4 + 4 == 8))
    assertions.toAssertion
  }

  it should "fail to flatten a strict sequence of assertions where one has failed" in {
    try {
      val assertions: List[org.scalatest.Assertion] =
        List(succeed, assert(4 + 4 == 7), assert(true))
      assertions.toAssertion
    } catch {
      case e: TestFailedException =>
        succeed
      case e: Throwable => fail
    }
  }

  it should "fail to flatten a lazy sequence of assertions where one has failed" in {
    try {
      val assertions: Stream[org.scalatest.Assertion] =
        (0 until 10).toStream.map { i =>
          if (i == 7) assert(false) else assert(true)
        }

      assertions.toAssertion
    } catch {
      case e: TestFailedException =>
        succeed
      case e: Throwable => fail
    }
  }

  it should "fail to flatten an empty list" in {
    intercept[IllegalArgumentException] {
      val xs = List.empty[org.scalatest.Assertion]
      xs.toAssertion
    }
  }

}
