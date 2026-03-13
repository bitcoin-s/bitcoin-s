package org.bitcoins.dlc.oracle.config

import org.bitcoins.dlc.oracle.DLCOracle
import org.bitcoins.keymanager.WalletStorage
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures.DLCOracleAppConfigFixture

import java.nio.file.Files

class DLCOracleAppConfigTest extends DLCOracleAppConfigFixture {

  behavior of "DLCOracleAppConfig"

  it must "initialize the oracle, move the seed somewhere else, and then start the oracle again and get the same pubkeys" in {
    dlcOracleAppConfig =>
      val seedFile = dlcOracleAppConfig.seedPath
      val dlcOracle = new DLCOracle()(dlcOracleAppConfig)
      val pubKey1 = dlcOracle.publicKey()

      // stop old oracle
      val stoppedF = for {
        _ <- dlcOracleAppConfig.stop()
      } yield ()

      // move the seed file to a new datadir
      val newDatadir = BitcoinSTestAppConfig.tmpDir()
      val newSeedPath = newDatadir
        .resolve("seeds")
        .resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)

      // create seed directory
      Files.createDirectories(newSeedPath.getParent)
      val copyF = stoppedF.map { _ =>
        // copy seed file to new directory
        Files.copy(seedFile, newSeedPath)
      }

      // start the new app config from the new datadir
      val appConfig = DLCOracleAppConfig
        .fromDatadir(newDatadir)

      val started2F = for {
        _ <- copyF
        _ <- appConfig.start()
      } yield ()

      val dlcOracle2F = started2F.map(_ => new DLCOracle()(appConfig))

      for {
        _ <- stoppedF
        pubKey2 <- dlcOracle2F.map(_.publicKey())
        // start the old app config for fixture tear down
        _ <- dlcOracleAppConfig.start()
      } yield {
        assert(pubKey1 == pubKey2)
      }
  }

  it must "fail to start the oracle app config if we have different seeds" in {
    dlcOracleAppConfig =>
      val seedFile = dlcOracleAppConfig.seedPath

      // stop old oracle
      val stoppedF = for {
        _ <- dlcOracleAppConfig.stop()
      } yield ()

      val deletedF = for {
        _ <- stoppedF
      } yield {
        // delete the seed so we start with a new seed
        Files.delete(seedFile)
      }

      val start2F = for {
        _ <- deletedF
        _ <- dlcOracleAppConfig.start()
      } yield ()

      // start it again and except an exception
      recoverToSucceededIf[RuntimeException] {
        start2F
      }
  }
}
