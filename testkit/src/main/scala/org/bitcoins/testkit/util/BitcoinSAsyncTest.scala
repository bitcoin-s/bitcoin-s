package org.bitcoins.testkit.util

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.util.Timeout
import org.bitcoins.core.config.{NetworkParameters, RegTest}
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.util.AsyncUtil
import org.scalacheck.{Gen, Shrink}
import org.scalactic.anyvals.PosInt
import org.scalatest._
import org.scalatest.concurrent.AsyncTimeLimitedTests
import org.scalatest.flatspec.{AsyncFlatSpec, FixtureAsyncFlatSpec}
import org.scalatest.matchers.must.Matchers
import org.scalatest.time.Span
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

/** This is a base trait in bitcoin-s for async tests
  */
trait BaseAsyncTest
    extends BeforeAndAfter
    with BeforeAndAfterAll
    with Matchers
    with ScalaCheckPropertyChecks
    with AsyncTimeLimitedTests
    with BitcoinSLogger { this: AsyncTestSuite =>

  implicit def np: NetworkParameters = RegTest

  implicit def chainParams: ChainParams = np.chainParams

  implicit lazy val akkaTimeout = Timeout(10.seconds)

  implicit val system: ActorSystem = {
    ActorSystem(s"${getClass.getSimpleName}-${System.currentTimeMillis()}")
  }

  /**
    * Needed because the default execution context will become overloaded
    * if we do not specify a unique execution context for each suite
    */
  implicit override def executionContext: ExecutionContext =
    system.dispatcher

  override lazy val timeLimit: Span = 5.minutes

  /** This def ensures that shrinks are disabled for all calls to forAll.
    *
    * If you want to enable shrinking for a specific test, introduce an
    * implicit val into that scope with type Shrink[T] where T is the type
    * of the generator you want to enable shrinking on.
    */
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny[T]

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system, verifySystemShutdown = true)
  }

  /** The configuration for property based tests in our testing suite
    * @see http://www.scalatest.org/user_guide/writing_scalacheck_style_properties
    */
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = {
    generatorDriveConfigOldCode
  }

  /** Sets the generator driven tests to perform the given amount of execs */
  def customGenDrivenConfig(executions: Int): PropertyCheckConfiguration = {
    PropertyCheckConfiguration(
      minSuccessful = PosInt.from(executions).get,
      minSize = PosInt.from(executions).get,
      workers = 2
    )
  }

  /** Property based tests that have been around a long time
    * have a less of a chance failing, so execute them less
    * @return
    */
  def generatorDriveConfigOldCode: PropertyCheckConfiguration = {
    customGenDrivenConfig(BitcoinSUnitTest.OLD_CODE_EXECUTIONS)
  }

  /** Property based tests that are new have a higher chance of failing
    * so execute them more
    * @return
    */
  def generatorDrivenConfigNewCode: PropertyCheckConfiguration = {
    customGenDrivenConfig(BitcoinSUnitTest.NEW_CODE_EXECUTIONS)
  }

  def sequenceTestRuns(
      testRunFs: Vector[Future[Assertion]]): Future[Assertion] = {
    val testRunsF: Future[Vector[Assertion]] =
      Future.sequence(testRunFs)

    testRunsF.map(_.reduce((_, testRun) => testRun))
  }

  def forAllAsync[A](gen: Gen[A])(
      func: A => Future[Assertion]): Future[Assertion] = {
    val testRunFs =
      new java.util.concurrent.CopyOnWriteArrayList[Future[Assertion]]

    forAll(gen) { input =>
      testRunFs.add(func(input))
      succeed
    }

    forAllHelper(testRunFs)
  }

  def forAllAsync[A, B](genA: Gen[A], genB: Gen[B])(
      func: (A, B) => Future[Assertion]): Future[Assertion] = {
    val testRunFs =
      new java.util.concurrent.CopyOnWriteArrayList[Future[Assertion]]

    forAll(genA, genB) {
      case (inputA, inputB) =>
        testRunFs.add(func(inputA, inputB))
        succeed
    }

    forAllHelper(testRunFs)
  }

  def forAllAsync[A, B, C](genA: Gen[A], genB: Gen[B], genC: Gen[C])(
      func: (A, B, C) => Future[Assertion]): Future[Assertion] = {
    val testRunFs =
      new java.util.concurrent.CopyOnWriteArrayList[Future[Assertion]]

    forAll(genA, genB, genC) {
      case (inputA, inputB, inputC) =>
        testRunFs.add(func(inputA, inputB, inputC))
        succeed
    }

    forAllHelper(testRunFs)
  }

  def forAllAsync[A, B, C, D](
      genA: Gen[A],
      genB: Gen[B],
      genC: Gen[C],
      genD: Gen[D])(
      func: (A, B, C, D) => Future[Assertion]): Future[Assertion] = {
    val testRunFs =
      new java.util.concurrent.CopyOnWriteArrayList[Future[Assertion]]

    forAll(genA, genB, genC, genD) {
      case (inputA, inputB, inputC, inputD) =>
        testRunFs.add(func(inputA, inputB, inputC, inputD))
        succeed
    }

    forAllHelper(testRunFs)
  }

  def forAllAsync[A, B, C, D, E](
      genA: Gen[A],
      genB: Gen[B],
      genC: Gen[C],
      genD: Gen[D],
      genE: Gen[E])(
      func: (A, B, C, D, E) => Future[Assertion]): Future[Assertion] = {
    val testRunFs =
      new java.util.concurrent.CopyOnWriteArrayList[Future[Assertion]]

    forAll(genA, genB, genC, genD, genE) {
      case (inputA, inputB, inputC, inputD, inputE) =>
        testRunFs.add(func(inputA, inputB, inputC, inputD, inputE))
        succeed
    }

    forAllHelper(testRunFs)
  }

  def forAllAsync[A, B, C, D, E, F](
      genA: Gen[A],
      genB: Gen[B],
      genC: Gen[C],
      genD: Gen[D],
      genE: Gen[E],
      genF: Gen[F])(
      func: (A, B, C, D, E, F) => Future[Assertion]): Future[Assertion] = {
    val testRunFs =
      new java.util.concurrent.CopyOnWriteArrayList[Future[Assertion]]

    forAll(genA, genB, genC, genD, genE, genF) {
      case (inputA, inputB, inputC, inputD, inputE, inputF) =>
        testRunFs.add(func(inputA, inputB, inputC, inputD, inputE, inputF))
        succeed
    }

    forAllHelper(testRunFs)
  }

  /** Runs all property based tests in parallel. This is a convenient optimization
    * for synchronous property based tests
    */
  def forAllParallel[A](gen: Gen[A])(
      func: A => Assertion): Future[Assertion] = {
    forAllAsync(gen) { a: A =>
      Future {
        func(a)
      }
    }
  }

  /** Runs all property based tests in parallel. This is a convenient optimization
    * for synchronous property based tests
    */
  def forAllParallel[A, B, C](genA: Gen[A], genB: Gen[B])(
      func: (A, B) => Assertion): Future[Assertion] = {
    forAllAsync(genA, genB) {
      case (inputA, inputB) =>
        Future {
          func(inputA, inputB)
        }
    }
  }

  /** Runs all property based tests in parallel. This is a convenient optimization
    * for synchronous property based tests
    */
  def forAllParallel[A, B, C](genA: Gen[A], genB: Gen[B], genC: Gen[C])(
      func: (A, B, C) => Assertion): Future[Assertion] = {
    forAllAsync(genA, genB, genC) {
      case (inputA, inputB, inputC) =>
        Future {
          func(inputA, inputB, inputC)
        }
    }
  }

  /** Runs all property based tests in parallel. This is a convenient optimization
    * for synchronous property based tests
    */
  def forAllParallel[A, B, C, D, E](
      genA: Gen[A],
      genB: Gen[B],
      genC: Gen[C],
      genD: Gen[D])(func: (A, B, C, D) => Assertion): Future[Assertion] = {
    forAllAsync(genA, genB, genC, genD) {
      case (inputA, inputB, inputC, inputD) =>
        Future {
          func(inputA, inputB, inputC, inputD)
        }
    }
  }

  /** Runs all property based tests in parallel. This is a convenient optimization
    * for synchronous property based tests
    */
  def forAllParallel[A, B, C, D, E](
      genA: Gen[A],
      genB: Gen[B],
      genC: Gen[C],
      genD: Gen[D],
      genE: Gen[E])(func: (A, B, C, D, E) => Assertion): Future[Assertion] = {
    forAllAsync(genA, genB, genC, genD, genE) {
      case (inputA, inputB, inputC, inputD, inputE) =>
        Future {
          func(inputA, inputB, inputC, inputD, inputE)
        }
    }
  }

  /** Runs all property based tests in parallel. This is a convenient optimization
    * for synchronous property based tests
    */
  def forAllParallel[A, B, C, D, E, F](
      genA: Gen[A],
      genB: Gen[B],
      genC: Gen[C],
      genD: Gen[D],
      genE: Gen[E],
      genF: Gen[F])(
      func: (A, B, C, D, E, F) => Assertion): Future[Assertion] = {
    forAllAsync(genA, genB, genC, genD, genE, genF) {
      case (inputA, inputB, inputC, inputD, inputE, inputF) =>
        Future {
          func(inputA, inputB, inputC, inputD, inputE, inputF)
        }
    }
  }

  /** Makes sure we have aggregated all of our test runs */
  private def forAllHelper(
      testRunsF: java.util.concurrent.CopyOnWriteArrayList[
        Future[Assertion]]): Future[Assertion] = {
    import akka.util.ccompat.JavaConverters._
    def helper(): Boolean = {
      testRunsF.size() == generatorDrivenConfig.minSize.value
    }
    for {
      _ <- AsyncUtil.retryUntilSatisfied(helper(), interval = 1.second)
      testRuns <- sequenceTestRuns(testRunsF.asScala.toVector)
    } yield {
      testRuns
    }
  }
}

/** A trait that uses [[AsyncFlatSpec]] to execute tests
  * This is different than [[BitcoinsBaseAsyncTest]] in the sense that
  * it extends [[AsyncFlatSpec]]. Some test cases in bitcoin-s we want
  * to provide fixtures, which means that suite needs to extend [[FixtureAsyncFlatSpec FixtureAsyncFlatSpec]]
  * to be able to use that fixture
  *
  * This test trait should be used for async tests that do NOT use a fixture.
  */
trait BitcoinSAsyncTest extends AsyncFlatSpec with BaseAsyncTest

/** A trait that uses [[FixtureAsyncFlatSpec AsyncFlatSpec]] to execute tests
  * This is different than [[BitcoinSAsyncTest BitcoinSAsyncTest]] as you can use a fixture
  * with this test suite.
  */
trait BitcoinSAsyncFixtureTest extends FixtureAsyncFlatSpec with BaseAsyncTest
