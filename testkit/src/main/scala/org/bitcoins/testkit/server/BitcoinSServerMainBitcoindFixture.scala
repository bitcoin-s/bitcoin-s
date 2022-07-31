package org.bitcoins.testkit.server

import org.bitcoins.commons.util.ServerArgParser
import org.bitcoins.server.BitcoinSServerMain
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.rpc.CachedBitcoindNewest
import org.bitcoins.testkit.wallet.{
  FundWalletUtil,
  WalletTestUtil,
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
        walletHolder <- server.start()
        account1 = WalletTestUtil.getHdAccount1(config.walletConf)

        //needed for fundWalletWithBitcoind
        _ <- walletHolder.createNewAccount(hdAccount = account1,
                                           keyManagerParams =
                                             walletHolder.keyManager.kmParams)
        walletWithBitcoind = WalletWithBitcoindRpc(walletHolder,
                                                   bitcoind,
                                                   config.walletConf)
        _ <- FundWalletUtil.fundWalletWithBitcoind(walletWithBitcoind)
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
