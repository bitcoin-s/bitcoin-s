package org.bitcoins.wallet

import com.typesafe.config.ConfigFactory
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.chain.MockChainQueryApi
import org.bitcoins.testkit.node.MockNodeApi
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.{BitcoinSTestAppConfig, PostgresTestDatabase}
import org.scalatest.Assertion

import scala.concurrent.Future

class MultiWalletTest extends BitcoinSAsyncTest with PostgresTestDatabase {

  it must "create 2 different wallets" in {
    val walletNameConfA =
      ConfigFactory.parseString(s"bitcoin-s.wallet.walletName = walletA")
    val walletNameConfB =
      ConfigFactory.parseString(s"bitcoin-s.wallet.walletName = walletB")

    val dbConf =
      BitcoinSTestAppConfig.configWithEmbeddedDb(project = None, postgresOpt)

    val dir = BitcoinSTestAppConfig.tmpDir()

    val configA =
      BitcoinSAppConfig(dir, Vector(walletNameConfA.withFallback(dbConf)))
    val configB =
      BitcoinSAppConfig(dir, Vector(walletNameConfB.withFallback(dbConf)))

    val walletAF =
      BitcoinSWalletTest.createDefaultWallet(MockNodeApi, MockChainQueryApi)(
        using configA.walletConf
      )

    val walletBF =
      BitcoinSWalletTest.createDefaultWallet(MockNodeApi, MockChainQueryApi)(
        using configB.walletConf
      )

    val assertionF: Future[Assertion] = for {
      walletA <- walletAF
      accountA <- walletA.accountHandling.getDefaultAccount()

      walletB <- walletBF
      accountB <- walletB.accountHandling.getDefaultAccount()
      _ <- walletA.walletConfig.stop()
      _ <- walletB.walletConfig.stop()
    } yield assert(accountA.xpub != accountB.xpub)

    assertionF
  }
}
