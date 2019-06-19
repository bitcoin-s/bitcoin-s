package org.bitcoins.testkit.fixtures

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest._
import org.bitcoins.wallet.models._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.db.WalletDbManagement
import slick.jdbc.SQLiteProfile

case class WalletDAOs(
    accountDAO: AccountDAO,
    addressDAO: AddressDAO,
    incomingTxDAO: IncomingTransactionDAO,
    outgoingTxDAO: OutgoingTransactionDAO,
    utxoDAO: UTXOSpendingInfoDAO)

trait WalletDAOFixture extends fixture.AsyncFlatSpec with BitcoinSWalletTest {

  private lazy val daos: WalletDAOs = {
    val account = AccountDAO()
    val address = AddressDAO()
    val inTx = IncomingTransactionDAO(SQLiteProfile)
    val outTx = OutgoingTransactionDAO(SQLiteProfile)
    val utxo = UTXOSpendingInfoDAO()
    WalletDAOs(account, address, inTx, outTx, utxo)
  }

  final override type FixtureParam = WalletDAOs

  implicit private val walletConfig: WalletAppConfig = config

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeFixture(build = () => WalletDbManagement.createAll().map(_ => daos),
                destroy = () => WalletDbManagement.dropAll())(test)
}
