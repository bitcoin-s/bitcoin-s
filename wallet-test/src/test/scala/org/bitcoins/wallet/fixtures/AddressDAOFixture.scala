package org.bitcoins.wallet.fixtures

import scala.concurrent.Future

import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.models.{AccountDAO, AddressDAO}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest._
import org.bitcoins.wallet.config.WalletAppConfig

/**
  * This fixture has a tuple of DAOs, because
  * addresses require an account to be valid
  */
trait AddressDAOFixture extends fixture.AsyncFlatSpec with BitcoinSWalletTest {

  final override type FixtureParam = (AccountDAO, AddressDAO)

  final override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeDependentFixture(createTables, dropTables)(test)

  // to get around the config in `BitcoinSWalletTest` not resolving
  // as an AppConfig
  implicit private val walletConfig: WalletAppConfig = config.walletConf

  private def dropTables(daos: FixtureParam): Future[Unit] = {
    val (account, address) = daos
    val dropAccountF = WalletDbManagement.dropTable(account.table)
    val dropAddressF = WalletDbManagement.dropTable(address.table)
    for {
      _ <- dropAccountF
      _ <- dropAddressF
    } yield ()

  }

  private def createTables(): Future[FixtureParam] = {
    val accountDAO = AccountDAO()
    val addressDAO = AddressDAO()

    val createAccountF =
      WalletDbManagement.createTable(accountDAO.table)
    val createTableF =
      WalletDbManagement.createTable(addressDAO.table)
    for {
      _ <- createAccountF
      _ <- createTableF
    } yield (accountDAO, addressDAO)
  }

}
