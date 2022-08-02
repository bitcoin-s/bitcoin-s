package org.bitcoins.server

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.cli.ConsoleCli.exec
import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.commons.rpc.{GetBalance, GetNewAddress}
import org.bitcoins.commons.util.ServerArgParser
import org.bitcoins.testkit.fixtures.BitcoinSAppConfigBitcoinFixtureNotStarted
import org.bitcoins.testkit.tor.CachedTor

import scala.concurrent.duration.DurationInt

/** Test starting bitcoin-s with bitcoind as the backend for app */
class BitcoinSServerMainBitcoindTorTest
    extends BitcoinSAppConfigBitcoinFixtureNotStarted
    with CachedTor {

  behavior of "BitcoinSServerMain"

  it must "start our app server with bitcoind as a backend with tor" in {
    config: BitcoinSAppConfig =>
      val server = new BitcoinSServerMain(ServerArgParser.empty)(system, config)

      val cliConfig = Config(rpcPortOpt = Some(config.rpcPort),
                             rpcPassword = config.rpcPassword)

      for {
        _ <- torF
        _ <- server.start()

        info = ConsoleCli.exec(CliCommand.WalletInfo, cliConfig)
        balance = exec(GetBalance(isSats = true), cliConfig)
        addr = exec(GetNewAddress(labelOpt = None), cliConfig)
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
}
