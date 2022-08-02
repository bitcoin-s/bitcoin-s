package org.bitcoins.server

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.commons.util.ServerArgParser
import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.testkit.fixtures.BitcoinSAppConfigBitcoinFixtureNotStarted

import scala.concurrent.duration.DurationInt

/** Test starting bitcoin-s with bitcoind as the backend for app */
class BitcoinSServerMainBitcoindTest
    extends BitcoinSAppConfigBitcoinFixtureNotStarted {

  behavior of "BitcoinSServerMain"

  it must "start our app server with bitcoind as a backend" in {
    config: BitcoinSAppConfig =>
      val server = new BitcoinSServerMain(ServerArgParser.empty)(system, config)

      val cliConfig = Config(rpcPortOpt = Some(config.rpcPort),
                             rpcPassword = config.rpcPassword)

      for {
        _ <- server.start()

        info = ConsoleCli.exec(CliCommand.WalletInfo, cliConfig)
        balance = ConsoleCli.exec(CliCommand.GetBalance(isSats = true),
                                  cliConfig)
        addr = ConsoleCli.exec(CliCommand.GetNewAddress(labelOpt = None),
                               cliConfig)
        blockHash = ConsoleCli.exec(CliCommand.GetBestBlockHash, cliConfig)
        _ <- AsyncUtil.nonBlockingSleep(1.second)
        _ <- server.stop() //stop to free all resources
      } yield {
        assert(info.isSuccess)
        assert(balance.isSuccess)
        assert(balance.get == "0")
        assert(addr.isSuccess)
        assert(blockHash.isSuccess)
      }
  }

  it must "load wallet" in { config: BitcoinSAppConfig =>
    val alice = None
    val bob = Some("bob")

    val server = new BitcoinSServerMain(ServerArgParser.empty)(system, config)

    val cliConfig = Config(rpcPortOpt = Some(config.rpcPort),
                           rpcPassword = config.rpcPassword)

    val mnemonic =
      MnemonicCode.fromEntropy(ECPrivateKey.freshPrivateKey.bytes.toBitVector)

    server.start().map { _ =>
      val info = ConsoleCli.exec(CliCommand.WalletInfo, cliConfig)
      assert(info.get.contains("\"walletName\": \"\""))
      val balance =
        ConsoleCli.exec(CliCommand.GetBalance(isSats = true), cliConfig)
      assert(balance.get == "0")
      val aliceAddr =
        ConsoleCli.exec(CliCommand.GetNewAddress(labelOpt = None), cliConfig)
      assert(aliceAddr.isSuccess)
      val aliceBlockHash =
        ConsoleCli.exec(CliCommand.GetBestBlockHash, cliConfig)
      assert(aliceBlockHash.isSuccess)

      // switch to bob
      val imported =
        ConsoleCli.exec(CliCommand.ImportSeed(bob, mnemonic, None), cliConfig)
      assert(imported.isSuccess)
      val bobLoaded =
        ConsoleCli.exec(CliCommand.LoadWallet(bob, None, None), cliConfig)
      assert(bobLoaded.get == "bob")
      val bobInfo = ConsoleCli.exec(CliCommand.WalletInfo, cliConfig)
      assert(bobInfo.get.contains("\"walletName\": \"bob\""))
      val bobAddr =
        ConsoleCli.exec(CliCommand.GetNewAddress(labelOpt = None), cliConfig)
      assert(bobAddr.isSuccess)
      val bobBlockHash = ConsoleCli.exec(CliCommand.GetBestBlockHash, cliConfig)
      assert(bobBlockHash.isSuccess)

      assert(aliceAddr != bobAddr)
      assert(aliceBlockHash == bobBlockHash)

      // switch back to alice
      val aliceLoaded =
        ConsoleCli.exec(CliCommand.LoadWallet(alice, None, None), cliConfig)
      assert(aliceLoaded.get == "")
      val aliceInfo = ConsoleCli.exec(CliCommand.WalletInfo, cliConfig)
      assert(aliceInfo.get.contains("\"walletName\": \"\""))
      val aliceAddresses =
        ConsoleCli.exec(CliCommand.GetUnusedAddresses, cliConfig)

      assert(aliceAddresses.get.contains(aliceAddr.get))
      assert(!aliceAddresses.get.contains(bobAddr.get))

      // again switch to bob
      val imported2 =
        ConsoleCli.exec(CliCommand.ImportSeed(bob, mnemonic, None), cliConfig)
      assert(imported2.isFailure)
      val bobLoaded2 =
        ConsoleCli.exec(CliCommand.LoadWallet(bob, None, None), cliConfig)
      assert(bobLoaded2.get == "bob")
      val bobInfo2 = ConsoleCli.exec(CliCommand.WalletInfo, cliConfig)
      assert(bobInfo2.get.contains("\"walletName\": \"bob\""))
      val bobAddresses =
        ConsoleCli.exec(CliCommand.GetUnusedAddresses, cliConfig)

      assert(!bobAddresses.get.contains(aliceAddr.get))
      assert(bobAddresses.get.contains(bobAddr.get))
    }
  }

  it must "fail to send requests to the app server if the password is bad" in {
    config: BitcoinSAppConfig =>
      val server = new BitcoinSServerMain(ServerArgParser.empty)(system, config)

      val cliConfig =
        Config(rpcPortOpt = Some(config.rpcPort), rpcPassword = "bad_password")

      val failF = for {
        _ <- server.start()
        infoT = ConsoleCli.exec(CliCommand.WalletInfo, cliConfig)
        _ <- AsyncUtil.nonBlockingSleep(1.second)
        _ <- server.stop()
      } yield {
        assert(infoT.isFailure)
        assert(
          infoT.failed.get.getMessage
            .contains("The supplied authentication is invalid"))
      }

      failF
  }
}
