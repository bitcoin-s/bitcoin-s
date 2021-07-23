package org.bitcoins.testkit.fixtures

import org.bitcoins.dlc.oracle.DLCOracle
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.testkit.oracle.OracleTestUtil
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.scalatest._

import scala.concurrent.Future

trait DLCOracleFixture extends BitcoinSFixture with EmbeddedPg {

  override type FixtureParam = DLCOracle

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[DLCOracle] = () => {
      val conf: DLCOracleAppConfig =
        BitcoinSTestAppConfig.getDLCOracleWithEmbeddedDbTestConfig(pgUrl)
      val _ = conf.migrate()

      val oracleF: Future[DLCOracle] = conf.initialize()
      oracleF
    }

    val destroy: DLCOracle => Future[Unit] = dlcOracle => {
      OracleTestUtil.destroyDLCOracleAppConfig(dlcOracle.conf)
    }
    makeDependentFixture(builder, destroy = destroy)(test)
  }
}
