package org.bitcoins.testkit.fixtures

import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.testkit.oracle.OracleTestUtil
import org.bitcoins.testkit.{BitcoinSTestAppConfig, PostgresTestDatabase}
import org.scalatest.FutureOutcome

import scala.concurrent.Future

trait DLCOracleAppConfigFixture
    extends BitcoinSFixture
    with PostgresTestDatabase {

  override type FixtureParam = DLCOracleAppConfig

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[DLCOracleAppConfig] = () => {
      val conf: DLCOracleAppConfig =
        BitcoinSTestAppConfig.getDLCOracleWithEmbeddedDbTestConfig(postgresOpt)
      val _ = conf.migrate()
      Future.successful(conf)
    }

    val destroy: DLCOracleAppConfig => Future[Unit] = dlcOracleAppConfig => {
      OracleTestUtil.destroyDLCOracleAppConfig(dlcOracleAppConfig)
    }
    makeDependentFixture(builder, destroy = destroy)(test)
  }
}
