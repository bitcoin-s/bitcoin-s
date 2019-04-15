package org.bitcoins.wallet.fixtures

import org.bitcoins.wallet.config.WalletDbManagement
import org.bitcoins.wallet.models.UTXOSpendingInfoDAO
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest._

import scala.concurrent.Future

trait UtxoDAOFixture extends fixture.AsyncFlatSpec with BitcoinSWalletTest {

  override final type FixtureParam = UTXOSpendingInfoDAO

  override final def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeDependentFixture(createUtxoTable, dropUtxoTable)(test)

  private def dropUtxoTable(utxoDAO: FixtureParam): Future[Unit] = {
    WalletDbManagement.dropTable(utxoDAO.table, dbConfig)
  }

  private def createUtxoTable(): Future[UTXOSpendingInfoDAO] = {
    val dao = UTXOSpendingInfoDAO(dbConfig)
    WalletDbManagement.createTable(dao.table, dbConfig).map(_ => dao)
  }

}
