package org.bitcoins.server

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.commons.util.ServerArgParser
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
