package org.bitcoins.testkit.server

import org.bitcoins.server.DLCWalletBitcoindBackendLoader
import org.bitcoins.server.util.WalletHolderWithBitcoindLoaderApi
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.rpc.CachedBitcoindNewest
import org.bitcoins.wallet.WalletHolder
import org.scalatest.FutureOutcome

import scala.concurrent.Future

trait WalletLoaderFixtures
    extends BitcoinSFixture
    with EmbeddedPg
    with CachedBitcoindNewest {

  def withBitcoindBackendLoader(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[WalletHolderWithBitcoindLoaderApi] = { () =>
      for {
        bitcoind <- cachedBitcoindWithFundsF
        config = BitcoinSServerMainUtil.buildBitcoindBitcoinSAppConfig(bitcoind)
        _ <- config.start()
        //initialize the default wallet so it can be used in tests
        _ <- config.walletConf.createHDWallet(nodeApi = bitcoind,
                                              chainQueryApi = bitcoind,
                                              feeRateApi = bitcoind)

        walletHolder = new WalletHolder()
        loader = DLCWalletBitcoindBackendLoader(
          walletHolder = walletHolder,
          bitcoind = bitcoind,
          nodeApi = bitcoind,
          feeProvider = bitcoind)(config, system)
      } yield WalletHolderWithBitcoindLoaderApi(walletHolder, loader)
    }

    val destroy: WalletHolderWithBitcoindLoaderApi => Future[Unit] = {
      walletHolderWithLoaderApi =>
        val loader = walletHolderWithLoaderApi.loaderApi
        val stopF = loader.stop()
        for {
          _ <- stopF
          _ <- BitcoinSServerMainUtil.destroyBitcoinSAppConfig(loader.conf)
        } yield ()
    }

    makeDependentFixture(builder, destroy)(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[EmbeddedPg].afterAll()
    super[BitcoinSFixture].afterAll()
  }
}
