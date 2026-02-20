package org.bitcoins.crypto

import org.bitcoins.testkitcore.Implicits.GeneratorOps
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

  implicit override def executionContext: ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

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

  def forAllAsync[A](
      gen: Gen[A]
  )(func: A => Future[Assertion]): Future[Assertion] = {

    val samples = 1
      .to(generatorDrivenConfig.minSuccessful)
      .map(_ => gen.sampleSome)
      .toVector

    val testRunsF = Future.traverse(samples)(func)

    checkRunResults(testRunsF)
  }

  def forAllAsync[A, B](genA: Gen[A], genB: Gen[B])(
      func: (A, B) => Future[Assertion]
  ): Future[Assertion] = {
    val samples = 1
      .to(generatorDrivenConfig.minSuccessful)
      .map(_ => (genA.sampleSome, genB.sampleSome))
      .toVector

    val testRunsF = Future.traverse(samples)(x => func(x._1, x._2))

    checkRunResults(testRunsF)
  }

  def forAllAsync[A, B, C](genA: Gen[A], genB: Gen[B], genC: Gen[C])(
      func: (A, B, C) => Future[Assertion]
  ): Future[Assertion] = {
    val samples = 1
      .to(generatorDrivenConfig.minSuccessful)
      .map(_ => (genA.sampleSome, genB.sampleSome, genC.sampleSome))
      .toVector

    val testRunsF = Future.traverse(samples)(x => func(x._1, x._2, x._3))

    checkRunResults(testRunsF)
  }

  def forAllAsync[A, B, C, D](
      genA: Gen[A],
      genB: Gen[B],
      genC: Gen[C],
      genD: Gen[D])(
      func: (A, B, C, D) => Future[Assertion]
  ): Future[Assertion] = {
    val samples = 1
      .to(generatorDrivenConfig.minSuccessful)
      .map(_ =>
        (genA.sampleSome, genB.sampleSome, genC.sampleSome, genD.sampleSome))
      .toVector

    val testRunsF = Future.traverse(samples)(x => func(x._1, x._2, x._3, x._4))

    checkRunResults(testRunsF)
  }

  private def checkRunResults(testRunsF: Future[Vector[Assertion]]) = {
    for {
      testRuns <- testRunsF
    } yield {
      val succeeded = testRuns.filter(_ == Succeeded)
      val failed = testRuns.filterNot(_ == Succeeded)
      if (succeeded.size < generatorDrivenConfig.minSuccessful) {
        failed.headOption.getOrElse(fail(
          s"succeeded.size=${succeeded.size} failed.size=${failed.size} minSuccessful=${generatorDrivenConfig.minSuccessful}"))
      } else {
        succeed
      }
    }
  }

}

object BitcoinSCryptoTest {

  /** The number of times new code should be executed in a property based test
    */
  val NEW_CODE_EXECUTIONS = 100

  /** The number of times old code should be executed in a property based test
    */
  val OLD_CODE_EXECUTIONS = 20

}
