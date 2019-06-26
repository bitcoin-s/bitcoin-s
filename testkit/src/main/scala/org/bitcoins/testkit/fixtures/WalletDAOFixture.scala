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
    incomingTxoDAO: IncomingTxoDAO,
    outgoingTxoDAO: OutgoingTxoDAO,
    utxoDAO: SpendingInfoDAO)

trait WalletDAOFixture extends fixture.AsyncFlatSpec with BitcoinSWalletTest {

  private lazy val daos: WalletDAOs = {
    val account = AccountDAO()
    val address = AddressDAO()
    val inTxo = IncomingTxoDAO(SQLiteProfile)
    val outTxo = OutgoingTxoDAO(SQLiteProfile)
    val utxo = SpendingInfoDAO()
    WalletDAOs(account, address, inTxo, outTxo, utxo)
  }

  final override type FixtureParam = WalletDAOs

  implicit private val walletConfig: WalletAppConfig = config

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeFixture(build = () => WalletDbManagement.createAll().map(_ => daos),
                destroy = () => WalletDbManagement.dropAll())(test)
}
