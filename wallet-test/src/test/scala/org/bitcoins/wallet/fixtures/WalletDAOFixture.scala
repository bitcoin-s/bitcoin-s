package org.bitcoins.wallet.fixtures

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest._
import org.bitcoins.wallet.models._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.db.WalletDbManagement

trait WalletDAOFixture extends fixture.AsyncFlatSpec with BitcoinSWalletTest {
  case class WalletDAOs(
      accountDAO: AccountDAO,
      addressDAO: AddressDAO,
      incomingTxDAO: IncomingTransactionDAO,
      outgoingTxDAO: OutgoingTransactionDAO,
      utxoDAO: UTXOSpendingInfoDAO)

  final override type FixtureParam = WalletDAOs

  implicit private val walletConfig: WalletAppConfig = config

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeFixture(build = WalletDbManagement.createAll,
                destroy = WalletDbManagement.dropAll)(test)
}
