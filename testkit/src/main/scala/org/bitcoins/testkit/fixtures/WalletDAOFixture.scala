package org.bitcoins.testkit.fixtures

import org.bitcoins.core.util.FutureUtil
import org.bitcoins.testkit.{BitcoinSTestAppConfig, PostgresTestDatabase}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models._
import org.scalatest._

import scala.concurrent.{Await, Future}

trait WalletDAOFixture extends BitcoinSFixture with PostgresTestDatabase {

  implicit protected val config: WalletAppConfig =
    BitcoinSTestAppConfig
      .getNeutrinoWithEmbeddedDbTestConfig(postgresOpt, Vector.empty)
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
      _ = config.clean()
    } yield ()
    res.failed.foreach(_.printStackTrace())
    res
  }

  override def afterAll(): Unit = {
    val stoppedF = config.stop()
    val _ = Await.ready(stoppedF, akkaTimeout.duration)
    super[PostgresTestDatabase].afterAll()
  }
}
