package org.bitcoins.testkit.fixtures

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.PostgresTestDatabase
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.*
import org.scalatest.*
trait WalletDAOFixture extends BitcoinSWalletTest with PostgresTestDatabase {

  final override type FixtureParam = WalletDAOs

  private def daos(implicit config: WalletAppConfig): WalletDAOs = {
    val account = AccountDAO()
    val address = AddressDAO()
    val tags = AddressTagDAO()
    val utxo = SpendingInfoDAO()
    val tx = TransactionDAO()
    val incomingTx = IncomingTransactionDAO()
    val outgoingTx = OutgoingTransactionDAO()
    val scriptPubKey = ScriptPubKeyDAO()
    val stateDescriptorDAO = WalletStateDescriptorDAO()
    WalletDAOs(
      account,
      address,
      tags,
      utxo,
      tx,
      incomingTx,
      outgoingTx,
      scriptPubKey,
      stateDescriptorDAO
    )
  }

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[WalletDAOs](
      () => {
        BitcoinSWalletTest
          .createWalletAppConfig(postgresOpt, Vector.empty)
          .map { wAppConfig =>
            daos(wAppConfig)
          }
      },
      { (daos: WalletDAOs) =>
        BitcoinSWalletTest.destroyWalletAppConfig(daos.walletConfig)
      }
    )(test)
  }
}
