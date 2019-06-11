package org.bitcoins.wallet.fixtures

import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.models.UTXOSpendingInfoDAO
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest._

import scala.concurrent.Future
import org.bitcoins.wallet.config.WalletAppConfig

trait UtxoDAOFixture extends fixture.AsyncFlatSpec with BitcoinSWalletTest {

  override final type FixtureParam = UTXOSpendingInfoDAO

  override final def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeDependentFixture(createUtxoTable, dropUtxoTable)(test)

  // to get around the config in `BitcoinSWalletTest` not resolving
  // as an AppConfig
  private implicit val walletConfig: WalletAppConfig = config.walletConf

  private def dropUtxoTable(utxoDAO: FixtureParam): Future[Unit] = {
    WalletDbManagement.dropTable(utxoDAO.table)
  }

  private def createUtxoTable(): Future[UTXOSpendingInfoDAO] = {
    val dao = UTXOSpendingInfoDAO()
    WalletDbManagement.createTable(dao.table).map(_ => dao)
  }

}
