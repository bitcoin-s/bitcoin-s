package org.bitcoins.wallet.fixtures

import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.api.{
  InitializeWalletError,
  InitializeWalletResult,
  InitializeWalletSuccess,
  UnlockedWalletApi
}
import org.bitcoins.wallet.config.WalletDbManagement
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest._

import scala.concurrent.{Await, Future}

trait WalletFixture
    extends AsyncFlatSpec
    with BeforeAndAfterAll
    with BitcoinSWalletTest {

  /** initialized wallet */
  lazy val walletF: Future[UnlockedWalletApi] =
    Wallet.initialize(chainParams, dbConfig).map {
      case InitializeWalletSuccess(wallet) => wallet
      case err: InitializeWalletError      => fail(err)
    }

  override protected def beforeAll(): Unit = {
    Await.result(WalletDbManagement.dropAll(dbConfig), timeout)
    Await.result(WalletDbManagement.createAll(dbConfig), timeout)
  }
}
