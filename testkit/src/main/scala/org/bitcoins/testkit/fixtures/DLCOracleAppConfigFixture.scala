package org.bitcoins.testkit.fixtures

import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.testkit.oracle.OracleTestUtil
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.scalatest.FutureOutcome

import scala.concurrent.Future

trait DLCOracleAppConfigFixture extends BitcoinSFixture with EmbeddedPg {

  override type FixtureParam = DLCOracleAppConfig

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[DLCOracleAppConfig] = () => {
      val conf: DLCOracleAppConfig =
        BitcoinSTestAppConfig.getDLCOracleWithEmbeddedDbTestConfig(pgUrl)
      val _ = conf.migrate()
      Future.successful(conf)
    }

    val destroy: DLCOracleAppConfig => Future[Unit] = dlcOracleAppConfig => {
      OracleTestUtil.destroyDLCOracleAppConfig(dlcOracleAppConfig)
    }
    makeDependentFixture(builder, destroy = destroy)(test)
  }
}
