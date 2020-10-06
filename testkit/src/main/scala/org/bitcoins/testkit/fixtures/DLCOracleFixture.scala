package org.bitcoins.testkit.fixtures

import org.bitcoins.crypto.AesPassword
import org.bitcoins.dlc.oracle.DLCOracle
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil.bip39PasswordOpt
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.scalatest._

import scala.concurrent.Future

trait DLCOracleFixture extends BitcoinSFixture with EmbeddedPg {

  override type FixtureParam = DLCOracle

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[DLCOracle] = () => {
      val conf =
        BitcoinSTestAppConfig.getDLCOracleWithEmbeddedDbTestConfig(pgUrl)
      conf.initialize(AesPassword.fromString("Ben was here"), bip39PasswordOpt)
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
