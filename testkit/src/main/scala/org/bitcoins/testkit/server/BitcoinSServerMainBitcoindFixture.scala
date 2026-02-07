package org.bitcoins.testkit.server

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.cli.{Config, ConsoleCli}
import org.bitcoins.commons.rpc.{CreateNewAccount, GetBalance, GetNewAddress}
import org.bitcoins.commons.util.ServerArgParser
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.server.BitcoinSServerMain
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.rpc.CachedBitcoindNewest
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, FundWalletUtil}
import org.scalatest.FutureOutcome

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

/** Starts an instnace of [[BitcoinSserverMain]] that is using bitcoind as a
  * backend
  */
trait BitcoinSServerMainBitcoindFixture
    extends BitcoinSFixture
    with EmbeddedPg
    with CachedBitcoindNewest {

  override type FixtureParam = ServerWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[ServerWithBitcoind] = () => {
      for {
        bitcoind <- cachedBitcoindWithFundsF
        config = BitcoinSServerMainUtil.buildBitcoindBitcoinSAppConfig(bitcoind)
        server = new BitcoinSServerMain(ServerArgParser.empty)(
          using system,
          config)
        _ <- server.start()

        // needed for fundWalletWithBitcoind
        cliConfig = Config(rpcPortOpt = Some(config.rpcPort),
                           rpcPassword = config.rpcPassword)
        _ = ConsoleCli.exec(CreateNewAccount(config.walletConf.defaultPurpose),
                            cliConfig)
        addresses = Vector
          .fill(3)(ConsoleCli.exec(GetNewAddress(None), cliConfig))
          .map(_.get)
          .map(BitcoinAddress.fromString)
        addressAmountMap = addresses
          .zip(BitcoinSWalletTest.defaultAcctAmts)
          .toMap
        _ <- FundWalletUtil.fundAddressesWithBitcoind(addressAmountMap,
                                                      bitcoind)
        // wait for funds to be credited
        _ <- AsyncUtil.retryUntilSatisfied(
          {
            val balance = ConsoleCli.exec(GetBalance(false), cliConfig)
            balance.get.toDouble != 0
          },
          interval = 1.second)
      } yield {
        ServerWithBitcoind(bitcoind, server)
      }
    }

    val destroy: ServerWithBitcoind => Future[Unit] = { serverWithBitcoind =>
      val stopF = serverWithBitcoind.server.stop()
      for {
        _ <- stopF
        _ <- BitcoinSServerMainUtil
          .destroyBitcoinSAppConfig(serverWithBitcoind.server.conf)
      } yield {
        ()
      }
    }

    makeDependentFixture(builder, destroy)(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[EmbeddedPg].afterAll()
    super[BitcoinSFixture].afterAll()
  }
}
