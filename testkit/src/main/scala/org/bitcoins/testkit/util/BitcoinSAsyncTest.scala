package org.bitcoins.testkit.util

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.util.Timeout
import org.bitcoins.core.config.{NetworkParameters, RegTest}
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.util.BitcoinSLogger
import org.scalatest._
import org.scalatest.concurrent.AsyncTimeLimitedTests
import org.scalatest.time.Span

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

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system, verifySystemShutdown = true)
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
