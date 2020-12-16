package org.bitcoins.testkit.fixtures

import org.bitcoins.core.util.FutureUtil
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
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
    scriptPubKeyDAO: ScriptPubKeyDAO,
    stateDescriptorDAO: WalletStateDescriptorDAO) {

  val list = Vector(scriptPubKeyDAO,
                    accountDAO,
                    addressDAO,
                    addressTagDAO,
                    transactionDAO,
                    incomingTxDAO,
                    utxoDAO,
                    outgoingTxDAO,
                    stateDescriptorDAO)
}

trait WalletDAOFixture extends BitcoinSFixture with EmbeddedPg {

  implicit protected val config: WalletAppConfig =
    BitcoinSTestAppConfig
      .getNeutrinoWithEmbeddedDbTestConfig(() => pgUrl())
      .walletConf

  final override type FixtureParam = WalletDAOs

  private lazy val daos: WalletDAOs = {
    val account = AccountDAO()
    val address = AddressDAO()
    val tags = AddressTagDAO()
    val utxo = SpendingInfoDAO()
    val tx = TransactionDAO()
    val incomingTx = IncomingTransactionDAO()
    val outgoingTx = OutgoingTransactionDAO()
    val scriptPubKey = ScriptPubKeyDAO()
    val stateDescriptorDAO = WalletStateDescriptorDAO()
    WalletDAOs(account,
               address,
               tags,
               utxo,
               tx,
               incomingTx,
               outgoingTx,
               scriptPubKey,
               stateDescriptorDAO)
  }

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(
      build = () => {
        Future(config.migrate()).map(_ => daos)
      },
      destroy = () => dropAll()
    )(test)
  }

  private def dropAll(): Future[Unit] = {
    val res = for {
      _ <- FutureUtil.sequentially(daos.list.reverse)(dao => dao.deleteAll())
    } yield ()
    res.failed.foreach(_.printStackTrace())
    res
  }
}
