package org.bitcoins.testkit.fixtures

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models._
import org.scalatest._
import org.scalatest.flatspec.FixtureAsyncFlatSpec

import scala.concurrent.Future

case class WalletDAOs(
    accountDAO: AccountDAO,
    addressDAO: AddressDAO,
    utxoDAO: SpendingInfoDAO,
    transactionDAO: TransactionDAO,
    incomingTxDAO: IncomingTransactionDAO,
    outgoingTxDAO: OutgoingTransactionDAO)

trait WalletDAOFixture extends FixtureAsyncFlatSpec with BitcoinSWalletTest {

  private lazy val daos: WalletDAOs = {
    val account = AccountDAO()
    val address = AddressDAO()
    val utxo = SpendingInfoDAO()
    val tx = TransactionDAO()
    val incomingTx = IncomingTransactionDAO()
    val outgoingTx = OutgoingTransactionDAO()
    WalletDAOs(account, address, utxo, tx, incomingTx, outgoingTx)
  }

  final override type FixtureParam = WalletDAOs

  implicit private val walletConfig: WalletAppConfig = config

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeFixture(build = () => Future(walletConfig.migrate()).map(_ => daos),
                destroy = () => dropAll())(test)

  def dropAll(): Future[Unit] =
    for {
      _ <- walletConfig.dropTable("flyway_schema_history")
      _ <- walletConfig.dropAll()
    } yield ()
}
