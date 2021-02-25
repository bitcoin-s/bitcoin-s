package org.bitcoins.testkit.util

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.util.Timeout
import org.bitcoins.testkitcore.util.BaseAsyncTest
import org.scalatest._
import org.scalatest.flatspec.{AsyncFlatSpec, FixtureAsyncFlatSpec}

import scala.concurrent.ExecutionContext

/** A bitcoin-s async test trait, that uses akka's actor system
  * execution context to run the scalatest test suites
  */
trait BitcoinSAkkaAsyncTest extends BaseAsyncTest { this: AsyncTestSuite =>
  implicit lazy val akkaTimeout = Timeout(duration)

  implicit val system: ActorSystem = {
    ActorSystem(s"${getClass.getSimpleName}-${System.currentTimeMillis()}")
  }

  /** Needed because the default execution context will become overloaded
    * if we do not specify a unique execution context for each suite
    */
  implicit override def executionContext: ExecutionContext =
    system.dispatcher

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system, verifySystemShutdown = true)
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
trait BitcoinSAsyncTest extends AsyncFlatSpec with BitcoinSAkkaAsyncTest

/** A trait that uses [[FixtureAsyncFlatSpec AsyncFlatSpec]] to execute tests
  * This is different than [[BitcoinSAsyncTest BitcoinSAsyncTest]] as you can use a fixture
  * with this test suite.
  */
trait BitcoinSAsyncFixtureTest
    extends FixtureAsyncFlatSpec
    with BitcoinSAkkaAsyncTest
