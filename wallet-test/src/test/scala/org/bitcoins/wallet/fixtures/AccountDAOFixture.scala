package org.bitcoins.wallet.fixtures

import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.models.AccountDAO
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest._

import scala.concurrent.Future
import org.bitcoins.wallet.config.WalletAppConfig

trait AccountDAOFixture extends fixture.AsyncFlatSpec with BitcoinSWalletTest {
  override final type FixtureParam = AccountDAO

  // to get around the config in `BitcoinSWalletTest` not resolving
  // as an AppConfig
  private implicit val walletConfig: WalletAppConfig = config.walletConf

  override final def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeDependentFixture(createAccountTable, dropAccountTable)(test)

  private def dropAccountTable(accountDAO: AccountDAO): Future[Unit] = {
    WalletDbManagement.dropTable(accountDAO.table)
  }

  private def createAccountTable(): Future[AccountDAO] = {
    val dao = AccountDAO()
    WalletDbManagement.createTable(dao.table).map(_ => dao)
  }

}
