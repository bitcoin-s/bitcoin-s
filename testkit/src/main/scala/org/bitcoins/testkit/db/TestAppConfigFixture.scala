package org.bitcoins.testkit.db

import org.bitcoins.testkit.BitcoinSTestAppConfig.ProjectType
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.scalatest.{BeforeAndAfterAll, FutureOutcome}
import org.scalatest.flatspec.FixtureAsyncFlatSpec

import scala.concurrent.Future

trait TestAppConfigFixture
    extends FixtureAsyncFlatSpec
    with BeforeAndAfterAll
    with BitcoinSFixture
    with EmbeddedPg {

  override type FixtureParam = TestAppConfig

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withTestAppConfig(test)
  }

  def withTestAppConfig(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(getFreshTestConfig, destroyTestConfig)(test)
  }

  def getFreshTestConfig(): Future[TestAppConfig] = {
    val configOverride = BitcoinSTestAppConfig.configWithEmbeddedDb(
      Some(ProjectType.Test),
      pgUrl = pgUrl)
    val config =
      TestAppConfig(BitcoinSTestAppConfig.tmpDir(), Vector(configOverride))

    val _ = config.migrate()
    config.start().map { _ =>
      config
    }
  }

  def destroyTestConfig(testConfig: TestAppConfig): Future[Unit] = {
    for {
      _ <- testConfig.dropTable("flyway_schema_history")
      _ <- testConfig.dropAll()
      _ <- testConfig.stop()
    } yield ()
  }
}
