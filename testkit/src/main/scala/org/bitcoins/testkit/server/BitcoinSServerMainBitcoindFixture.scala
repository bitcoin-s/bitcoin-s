package org.bitcoins.testkit.server

import org.bitcoins.commons.util.ServerArgParser
import org.bitcoins.server.BitcoinSServerMain
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.rpc.CachedBitcoindNewest
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTest,
  FundWalletUtil,
  WalletWithBitcoindRpc
}
import org.scalatest.FutureOutcome

import scala.concurrent.Future

/** Starts an instnace of [[BitcoinSserverMain]] that is
  * using bitcoind as a backend
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
        server = new BitcoinSServerMain(ServerArgParser.empty)(system, config)
        _ <- server.start()
        //need to create account 2 to use FundWalletUtil.fundWalletWithBitcoind
        wallet <- BitcoinSWalletTest.createDLCWallet2Accounts(
          nodeApi = bitcoind,
          chainQueryApi = bitcoind,
          bip39PasswordOpt = None,
          extraConfig = None)(server.conf, system)
        _ <- FundWalletUtil.fundWalletWithBitcoind(
          WalletWithBitcoindRpc(wallet, bitcoind))
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
      } yield ()
    }

    makeDependentFixture(builder, destroy)(test)
  }
}
