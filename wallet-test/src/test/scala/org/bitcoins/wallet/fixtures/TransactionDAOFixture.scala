package org.bitcoins.wallet.fixtures

import org.scalatest._
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.db.WalletDbManagement
import slick.jdbc.SQLiteProfile
import org.bitcoins.wallet.models.IncomingTransactionDAO
import org.bitcoins.wallet.models.OutgoingTransactionDAO
import org.bitcoins.wallet.models.UTXOSpendingInfoDAO
import org.bitcoins.wallet.models.AddressDAO

trait IncomingTransactionDAOFixture
    extends fixture.AsyncFlatSpec
    with BitcoinSWalletTest {
  final override type FixtureParam =
    (IncomingTransactionDAO, AddressDAO)

  final override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture[FixtureParam](
      build = {
        val txDao = IncomingTransactionDAO(SQLiteProfile)
        val addressDAO = AddressDAO()
        () =>
          WalletDbManagement.createAll().map(_ => (txDao, addressDAO))
      },
      destroy = WalletDbManagement.dropAll
    )(test)
  }

  implicit private val walletConfig: WalletAppConfig = config.walletConf

}

trait OutgoingTransactionDAOFixture
    extends fixture.AsyncFlatSpec
    with BitcoinSWalletTest {
  final override type FixtureParam = OutgoingTransactionDAO

  final override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[OutgoingTransactionDAO](
      build = {
        val dao = OutgoingTransactionDAO(SQLiteProfile)
        () =>
          WalletDbManagement.createTable(dao.table).map(_ => dao)
      },
      destroy = dao => WalletDbManagement.dropTable(dao.table)
    )(test)
  }

  implicit private val walletConfig: WalletAppConfig = config.walletConf

}
