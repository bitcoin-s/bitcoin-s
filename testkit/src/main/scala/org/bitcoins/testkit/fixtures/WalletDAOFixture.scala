package org.bitcoins.testkit.fixtures

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models._
import org.scalatest._

import scala.concurrent.Future

case class WalletDAOs(
    accountDAO: AccountDAO,
    addressDAO: AddressDAO,
    addressTagDAO: AddressTagDAO,
    utxoDAO: SpendingInfoDAO,
    transactionDAO: TransactionDAO,
    incomingTxDAO: IncomingTransactionDAO,
    outgoingTxDAO: OutgoingTransactionDAO,
    scriptPubKeyDAO: ScriptPubKeyDAO)

trait WalletDAOFixture extends BitcoinSWalletTest {

  private lazy val daos: WalletDAOs = {
    val account = AccountDAO()
    val address = AddressDAO()
    val tags = AddressTagDAO()
    val utxo = SpendingInfoDAO()
    val tx = TransactionDAO()
    val incomingTx = IncomingTransactionDAO()
    val outgoingTx = OutgoingTransactionDAO()
    val scriptPubKey = ScriptPubKeyDAO()
    WalletDAOs(account,
               address,
               tags,
               utxo,
               tx,
               incomingTx,
               outgoingTx,
               scriptPubKey)
  }

  final override type FixtureParam = WalletDAOs

  implicit private val walletConfig: WalletAppConfig = config

  override def afterAll(): Unit = {
    super.afterAll()
  }

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeFixture(build = () => Future(walletConfig.migrate()).map(_ => daos),
                destroy = () => dropAll())(test)

  def dropAll(): Future[Unit] = {
    val res = for {
      _ <- walletConfig.dropTable("flyway_schema_history")
      _ <- walletConfig.dropAll()
    } yield ()
    res.failed.foreach { ex =>
      ex.printStackTrace()
    }
    res
  }

}
