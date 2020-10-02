package org.bitcoins.testkit.fixtures

import org.bitcoins.crypto.AesPassword
import org.bitcoins.dlc.oracle.{DLCOracle, DLCOracleAppConfig}
import org.bitcoins.testkit.BitcoinSTestAppConfig.tmpDir
import org.bitcoins.testkit.util.FileUtil
import org.scalatest._

import scala.concurrent.Future

trait DLCOracleFixture extends BitcoinSFixture {

  override type FixtureParam = DLCOracle

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[DLCOracle] = () => {
      val conf = DLCOracleAppConfig(tmpDir())
      conf.initialize(AesPassword.fromString("Ben was here"), None)
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
