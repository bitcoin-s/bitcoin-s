package org.bitcoins.wallet.fixtures

import scala.concurrent.Future

import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.models.{AccountDAO, AddressDAO}
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest._

/**
  * This fixture has a tuple of DAOs, because
  * addresses require an account to be valid
  */
trait AddressDAOFixture extends fixture.AsyncFlatSpec with BitcoinSWalletTest {

  override final type FixtureParam = (AccountDAO, AddressDAO)

  override final def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeDependentFixture(createTables, dropTables)(test)

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
