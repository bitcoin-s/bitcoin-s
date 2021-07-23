package org.bitcoins.dlc.oracle.config

import org.bitcoins.keymanager.WalletStorage
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures.DLCOracleAppConfigFixture

import java.nio.file.Files

class DLCOracleAppConfigTest extends DLCOracleAppConfigFixture {

  behavior of "DLCOracleAppConfig"

  it must "initialize the same oracle twice" in {
    dlcOracleAppConfig: DLCOracleAppConfig =>
      val dlcOracle1F = dlcOracleAppConfig.initialize()
      val dlcOracle2F = dlcOracleAppConfig.initialize()

      for {
        dlcOracle1 <- dlcOracle1F
        dlcOracle2 <- dlcOracle2F
      } yield {
        assert(dlcOracle1.publicKey == dlcOracle2.publicKey)
      }
  }

  it must "initialize the oracle, move the seed somewhere else, and then start the oracle again and get the same pubkeys" in {
    dlcOracleAppConfig: DLCOracleAppConfig =>
      val seedFile = dlcOracleAppConfig.seedPath
      val dlcOracle1F = dlcOracleAppConfig.initialize()
      val pubKeyBeforeMoveF = dlcOracle1F.map(_.publicKey)

      //stop old oracle
      val stoppedF = for {
        _ <- dlcOracle1F
        _ <- dlcOracleAppConfig.stop()
      } yield ()

      //move the seed file to a new datadir
      val newDatadir = BitcoinSTestAppConfig.tmpDir()
      val newSeedPath = newDatadir
        .resolve("seeds")
        .resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)

      //create seed directory
      Files.createDirectories(newSeedPath.getParent)
      //copy seed file to new directory
      Files.copy(seedFile, newSeedPath)

      //start the new app config from the new datadir
      val dlcOracle2F = DLCOracleAppConfig
        .fromDatadir(newDatadir)
        .initialize()

      for {
        _ <- stoppedF
        pubKey1 <- pubKeyBeforeMoveF
        pubKey2 <- dlcOracle2F.map(_.publicKey)
      } yield {
        assert(pubKey1 == pubKey2)
      }
  }
}
