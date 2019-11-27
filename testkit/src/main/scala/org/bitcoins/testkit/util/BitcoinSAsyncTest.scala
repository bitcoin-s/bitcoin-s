package org.bitcoins.testkit.util

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.util.Timeout
import org.bitcoins.core.config.{NetworkParameters, RegTest}
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.Shrink
import org.scalactic.anyvals.PosInt
import org.scalatest._
import org.scalatest.concurrent.AsyncTimeLimitedTests
import org.scalatest.time.Span
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

/** This is a base trait in bitcoin-s for async tests
  *
  *
  * */
trait BaseAsyncTest
    extends BeforeAndAfter
    with BeforeAndAfterAll
    with MustMatchers
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
}

/** A trait that uses [[AsyncFlatSpec]] to execute tests
  * This is different than [[BitcoinsBaseAsyncTest]] in the sense that
  * it extends [[AsyncFlatSpec]]. Some test cases in bitcoin-s we want
  * to provide fixtures, which means that suite needs to extend [[org.scalatest.fixture.AsyncFlatSpec fixture.AsyncFlatSpec]]
  * to be able to use that fixture
  *
  * This test trait should be used for async tests that do NOT use a fixture.
  * */
trait BitcoinSAsyncTest extends AsyncFlatSpec with BaseAsyncTest

/** A trait that uses [[fixture.AsyncFlatSpec fixture.AsyncFlatSpec]] to execute tests
  * This is different than [[BitcoinSAsyncTest BitcoinSAsyncTest]] as you can use a fixture
  * with this test suite.
  *
  * */
trait BitcoinSAsyncFixtureTest extends fixture.AsyncFlatSpec with BaseAsyncTest
