package org.bitcoins.wallet.fixtures

import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.models.AccountDAO
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest._

import scala.concurrent.Future
import org.bitcoins.wallet.config.WalletAppConfig

trait AccountDAOFixture extends fixture.AsyncFlatSpec with BitcoinSWalletTest {
  final override type FixtureParam = AccountDAO

  // to get around the config in `BitcoinSWalletTest` not resolving
  // as an AppConfig
  implicit private val walletConfig: WalletAppConfig = config.walletConf

  final override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeDependentFixture(createAccountTable, dropAccountTable)(test)

  private def dropAccountTable(accountDAO: AccountDAO): Future[Unit] = {
    WalletDbManagement.dropTable(accountDAO.table)
  }

  private def createAccountTable(): Future[AccountDAO] = {
    val dao = AccountDAO()
    WalletDbManagement.createTable(dao.table).map(_ => dao)
  }

}
