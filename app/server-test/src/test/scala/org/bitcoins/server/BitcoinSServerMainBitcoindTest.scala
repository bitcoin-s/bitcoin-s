package org.bitcoins.server

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.cli.CliCommand.GetBestBlockHash
import org.bitcoins.cli.ConsoleCli.exec
import org.bitcoins.commons.rpc.{
  GetBalance,
  GetNewAddress,
  ImportSeed,
  LoadWallet
}
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

  it must "fail to send requests to the app server if the password is bad" in {
    (config: BitcoinSAppConfig) =>
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
            .contains("The supplied authentication is invalid")
        )
      }

      failF
  }

  it must "start our app server with bitcoind as a backend" in {
    (config: BitcoinSAppConfig) =>
      val server = new BitcoinSServerMain(ServerArgParser.empty)(system, config)

      val cliConfig = Config(
        rpcPortOpt = Some(config.rpcPort),
        rpcPassword = config.rpcPassword
      )

      for {
        _ <- server.start()

        info = ConsoleCli.exec(CliCommand.WalletInfo, cliConfig)
        balance = exec(GetBalance(isSats = true), cliConfig)
        addr = exec(GetNewAddress(labelOpt = None), cliConfig)
        blockHash = ConsoleCli.exec(CliCommand.GetBestBlockHash, cliConfig)
        _ <- AsyncUtil.nonBlockingSleep(1.second)
        _ <- server.stop() // stop to free all resources
      } yield {
        assert(info.isSuccess)
        assert(balance.isSuccess)
        assert(balance.get == "0")
        assert(addr.isSuccess)
        assert(blockHash.isSuccess)
      }
  }

  it must "load wallet" in { (config: BitcoinSAppConfig) =>
    val alice = None
    val bob = Some("bob")

    val server = new BitcoinSServerMain(ServerArgParser.empty)(system, config)

    val cliConfig = Config(
      rpcPortOpt = Some(config.rpcPort),
      rpcPassword = config.rpcPassword
    )

    val mnemonic =
      MnemonicCode.fromEntropy(ECPrivateKey.freshPrivateKey.bytes.toBitVector)

    for {
      _ <- server.start()
      info = ConsoleCli.exec(CliCommand.WalletInfo, cliConfig)
      _ = assert(info.get.contains("\"walletName\": \"\""))
      balance =
        ConsoleCli.exec(GetBalance(isSats = true), cliConfig)
      _ = assert(balance.get == "0")
      aliceAddr =
        ConsoleCli.exec(GetNewAddress(labelOpt = None), cliConfig)
      _ = assert(aliceAddr.isSuccess)
      aliceBlockHash =
        ConsoleCli.exec(GetBestBlockHash, cliConfig)
      _ = assert(aliceBlockHash.isSuccess)
      imported =
        ConsoleCli.exec(ImportSeed(bob, mnemonic, None), cliConfig)
      _ = assert(imported.isSuccess)
      bobLoaded =
        ConsoleCli.exec(LoadWallet(bob, None, None), cliConfig)
      _ = assert(bobLoaded.get == "bob")
      bobInfo = ConsoleCli.exec(CliCommand.WalletInfo, cliConfig)
      _ = assert(bobInfo.get.contains("\"walletName\": \"bob\""))
      bobAddr = ConsoleCli.exec(GetNewAddress(labelOpt = None), cliConfig)
      _ = assert(bobAddr.isSuccess)
      bobBlockHash = ConsoleCli.exec(CliCommand.GetBestBlockHash, cliConfig)
      _ = assert(bobBlockHash.isSuccess)

      _ = assert(aliceAddr != bobAddr)
      _ = assert(aliceBlockHash == bobBlockHash)

      // switch back to alice
      aliceLoaded =
        ConsoleCli.exec(LoadWallet(alice, None, None), cliConfig)
      _ = assert(aliceLoaded.get == "")
      aliceInfo = ConsoleCli.exec(CliCommand.WalletInfo, cliConfig)
      _ = assert(aliceInfo.get.contains("\"walletName\": \"\""))
      aliceAddresses = ConsoleCli.exec(CliCommand.GetUnusedAddresses, cliConfig)

      _ = assert(aliceAddresses.get.contains(aliceAddr.get))
      _ = assert(!aliceAddresses.get.contains(bobAddr.get))

      // again switch to bob
      imported2 =
        ConsoleCli.exec(ImportSeed(bob, mnemonic, None), cliConfig)
      _ = assert(!upickle.default.read[Boolean](imported2.get))
      bobLoaded2 =
        ConsoleCli.exec(LoadWallet(bob, None, None), cliConfig)
      _ = assert(bobLoaded2.get == "bob")
      bobInfo2 = ConsoleCli.exec(CliCommand.WalletInfo, cliConfig)
      _ = assert(bobInfo2.get.contains("\"walletName\": \"bob\""))
      bobAddresses =
        ConsoleCli.exec(CliCommand.GetUnusedAddresses, cliConfig)

      _ = assert(!bobAddresses.get.contains(aliceAddr.get))
      _ = assert(bobAddresses.get.contains(bobAddr.get))
      _ <- server.stop()
    } yield succeed
  }
}
