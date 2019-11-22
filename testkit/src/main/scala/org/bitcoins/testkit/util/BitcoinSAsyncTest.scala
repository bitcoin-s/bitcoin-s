package org.bitcoins.testkit.util

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.util.Timeout
import org.bitcoins.core.config.{NetworkParameters, RegTest}
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Gen, Shrink}
import org.scalactic.anyvals.PosInt
import org.scalatest._
import org.scalatest.flatspec.FixtureAsyncFlatSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.concurrent.AsyncTimeLimitedTests
import org.scalatest.time.Span
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.DurationInt

/** This is a base trait in bitcoin-s for async tests
  *
  *
  * */
trait BaseAsyncTest
    extends BeforeAndAfter
    with BeforeAndAfterAll
    with Matchers
    with AsyncTimeLimitedTests
    with ScalaCheckPropertyChecks
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
  implicit override lazy val executionContext: ExecutionContext =
    system.dispatcher

  override lazy val timeLimit: Span = 5.minutes

  /** This def ensures that shrinks are disabled for all calls to forAll.
    *
    * If you want to enable shrinking for a specific test, introduce an
    * implicit val into that scope with type Shrink[T] where T is the type
    * of the generator you want to enable shrinking on.
    */
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny[T]

  override def afterAll: Unit = {
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
      testRunFs: scala.collection.mutable.Builder[
        Future[Assertion],
        Vector[Future[Assertion]]]): Future[Assertion] = {
    val testRunsF: Future[Vector[Assertion]] =
      Future.sequence(testRunFs.result())

    testRunsF.map(_.reduce((_, testRun) => testRun))
  }

  def forAllAsync[A](gen: Gen[A])(
      func: A => Future[Assertion]): Future[Assertion] = {
    val testRunFs = Vector.newBuilder[Future[Assertion]]

    forAll(gen) { input =>
      testRunFs.+=(func(input))
      succeed
    }

    sequenceTestRuns(testRunFs)
  }

  def forAllAsync[A, B](genA: Gen[A], genB: Gen[B])(
      func: (A, B) => Future[Assertion]): Future[Assertion] = {
    val testRunFs = Vector.newBuilder[Future[Assertion]]

    forAll(genA, genB) {
      case (inputA, inputB) =>
        testRunFs.+=(func(inputA, inputB))
        succeed
    }

    sequenceTestRuns(testRunFs)
  }

  def forAllAsync[A, B, C](genA: Gen[A], genB: Gen[B], genC: Gen[C])(
      func: (A, B, C) => Future[Assertion]): Future[Assertion] = {
    val testRunFs = Vector.newBuilder[Future[Assertion]]

    forAll(genA, genB, genC) {
      case (inputA, inputB, inputC) =>
        testRunFs.+=(func(inputA, inputB, inputC))
        succeed
    }

    sequenceTestRuns(testRunFs)
  }

  def forAllAsync[A, B, C, D](
      genA: Gen[A],
      genB: Gen[B],
      genC: Gen[C],
      genD: Gen[D])(
      func: (A, B, C, D) => Future[Assertion]): Future[Assertion] = {
    val testRunFs = Vector.newBuilder[Future[Assertion]]

    forAll(genA, genB, genC, genD) {
      case (inputA, inputB, inputC, inputD) =>
        testRunFs.+=(func(inputA, inputB, inputC, inputD))
        succeed
    }

    sequenceTestRuns(testRunFs)
  }

  def forAllAsync[A, B, C, D, E](
      genA: Gen[A],
      genB: Gen[B],
      genC: Gen[C],
      genD: Gen[D],
      genE: Gen[E])(
      func: (A, B, C, D, E) => Future[Assertion]): Future[Assertion] = {
    val testRunFs = Vector.newBuilder[Future[Assertion]]

    forAll(genA, genB, genC, genD, genE) {
      case (inputA, inputB, inputC, inputD, inputE) =>
        testRunFs.+=(func(inputA, inputB, inputC, inputD, inputE))
        succeed
    }

    sequenceTestRuns(testRunFs)
  }

  def forAllAsync[A, B, C, D, E, F](
      genA: Gen[A],
      genB: Gen[B],
      genC: Gen[C],
      genD: Gen[D],
      genE: Gen[E],
      genF: Gen[F])(
      func: (A, B, C, D, E, F) => Future[Assertion]): Future[Assertion] = {
    val testRunFs = Vector.newBuilder[Future[Assertion]]

    forAll(genA, genB, genC, genD, genE, genF) {
      case (inputA, inputB, inputC, inputD, inputE, inputF) =>
        testRunFs.+=(func(inputA, inputB, inputC, inputD, inputE, inputF))
        succeed
    }

    sequenceTestRuns(testRunFs)
  }
}

/** A trait that uses [[AsyncFlatSpec]] to execute tests
  * This is different than [[BitcoinsBaseAsyncTest]] in the sense that
  * it extends [[AsyncFlatSpec]]. Some test cases in bitcoin-s we want
  * to provide fixtures, which means that suite needs to extend [[FixtureAsyncFlatSpec FixtureAsyncFlatSpec]]
  * to be able to use that fixture
  *
  * This test trait should be used for async tests that do NOT use a fixture.
  * */
trait BitcoinSAsyncTest extends AsyncFlatSpec with BaseAsyncTest

/** A trait that uses [[FixtureAsyncFlatSpec AsyncFlatSpec]] to execute tests
  * This is different than [[BitcoinSAsyncTest BitcoinSAsyncTest]] as you can use a fixture
  * with this test suite.
  *
  * */
trait BitcoinSAsyncFixtureTest extends FixtureAsyncFlatSpec with BaseAsyncTest
