package org.bitcoins.testkit.fixtures

import org.bitcoins.dlc.oracle.DLCOracle
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.scalatest._

import scala.concurrent.Future

trait DLCOracleFixture extends BitcoinSFixture with EmbeddedPg {

  override type FixtureParam = DLCOracle

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[DLCOracle] = () => {
      println(s"builder")
      val conf: DLCOracleAppConfig =
        BitcoinSTestAppConfig.getDLCOracleWithEmbeddedDbTestConfig(pgUrl)
      println(s"Done with conf")
      val _ = conf.migrate()

      println(s"Done with migration")
      val oracleF: Future[DLCOracle] = conf.initialize()
      println(s"done with init")
      oracleF
    }

    val destroy: DLCOracle => Future[Unit] = dlcOracle => {
      val conf = dlcOracle.conf
      conf.dropAll().flatMap { _ =>
        FileUtil.deleteTmpDir(conf.baseDatadir)
        conf.stop()
      }
    }
    makeDependentFixture(builder, destroy = destroy)(test)
  }
}
