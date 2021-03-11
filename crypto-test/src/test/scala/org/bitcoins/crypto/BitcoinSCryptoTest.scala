package org.bitcoins.crypto

import org.scalacheck.Gen
import org.scalactic.anyvals.PosInt
import org.scalatest.flatspec.{AnyFlatSpec, AsyncFlatSpec}
import org.scalatest.matchers.must.Matchers
import org.scalatest.{Assertion, BeforeAndAfter, BeforeAndAfterAll, Succeeded}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.{ExecutionContext, Future}

trait BitcoinSCryptoTest
    extends AnyFlatSpec
    with BeforeAndAfter
    with BeforeAndAfterAll
    with Matchers
    with ScalaCheckPropertyChecks {

  implicit def executionContext: ExecutionContext =
    scala.concurrent.ExecutionContext.global

  def generatorDrivenConfigNewCode: PropertyCheckConfiguration = {
    customGenDrivenConfig(BitcoinSCryptoTest.NEW_CODE_EXECUTIONS)
  }

  /** Sets the generator driven tests to perform the given amount of execs */
  def customGenDrivenConfig(executions: Int): PropertyCheckConfiguration = {
    PropertyCheckConfiguration(
      minSuccessful = PosInt.from(executions).get,
      minSize = PosInt.from(executions).get,
      workers = 1
    )
  }

}

trait BitcoinSCryptoAsyncTest
    extends AsyncFlatSpec
    with BeforeAndAfter
    with BeforeAndAfterAll
    with Matchers
    with ScalaCheckPropertyChecks {

  implicit override def executionContext =
    scala.concurrent.ExecutionContext.Implicits.global

  def generatorDrivenConfigNewCode: PropertyCheckConfiguration = {
    customGenDrivenConfig(BitcoinSCryptoTest.NEW_CODE_EXECUTIONS)
  }

  /** Sets the generator driven tests to perform the given amount of execs */
  def customGenDrivenConfig(executions: Int): PropertyCheckConfiguration = {
    PropertyCheckConfiguration(
      minSuccessful = PosInt.from(executions).get,
      minSize = PosInt.from(executions).get,
      workers = 1
    )
  }

  def forAllAsync[A](gen: Gen[A])(
      func: A => Future[Assertion]): Future[Assertion] = {

    val samples = 1
      .to(generatorDrivenConfig.minSize)
      .map(_ => gen.sample)
      .toVector
      .flatten

    val testRunsF = Future.sequence(samples.map(func))

    checkRunResults(testRunsF)
  }

  def forAllAsync[A, B](genA: Gen[A], genB: Gen[B])(
      func: (A, B) => Future[Assertion]): Future[Assertion] = {

    val samples = 1
      .to(generatorDrivenConfig.minSize)
      .map(_ => (genA.sample, genB.sample))
      .toVector
      .collect { case (Some(a), Some(b)) =>
        (a, b)
      }

    val testRunsF = Future.sequence(samples.map(x => func(x._1, x._2)))

    checkRunResults(testRunsF)
  }

  private def checkRunResults(testRunsF: Future[Vector[Assertion]]) = {
    for {
      testRuns <- testRunsF
    } yield {
      val succeeded = testRuns.filter(_ == Succeeded)
      val failed = testRuns.filterNot(_ == Succeeded)
      if (succeeded.size < generatorDrivenConfig.minSuccessful) {
        failed.headOption.getOrElse(fail())
      } else {
        succeed
      }
    }
  }

}

object BitcoinSCryptoTest {

  /** The number of times new code
    * should be executed in a property based test
    */
  val NEW_CODE_EXECUTIONS = 100

  /** The number of times old code should be executed
    * in a property based test
    */
  val OLD_CODE_EXECUTIONS = 20

}
