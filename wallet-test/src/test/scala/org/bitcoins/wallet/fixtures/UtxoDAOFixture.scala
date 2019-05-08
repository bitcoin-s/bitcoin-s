package org.bitcoins.wallet.fixtures

import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.models.UTXOSpendingInfoDAO
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest._

import scala.concurrent.Future

trait UtxoDAOFixture extends fixture.AsyncFlatSpec with BitcoinSWalletTest {

  override final type FixtureParam = UTXOSpendingInfoDAO

  override final def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeDependentFixture(createUtxoTable, dropUtxoTable)(test)

  private def dropUtxoTable(utxoDAO: FixtureParam): Future[Unit] = {
    WalletDbManagement.dropTable(utxoDAO.table)
  }

  private def createUtxoTable(): Future[UTXOSpendingInfoDAO] = {
    val dao = UTXOSpendingInfoDAO()
    WalletDbManagement.createTable(dao.table).map(_ => dao)
  }

}
