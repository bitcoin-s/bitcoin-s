package org.bitcoins.wallet.util

import akka.actor.ActorSystem
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.{CRUD, UnitTestDbConfig}
import org.bitcoins.wallet.config.WalletDbManagement
import org.bitcoins.wallet.models.{
  AccountDAO,
  AddressDAO,
  MnemonicCodeDAO,
  UTXOSpendingInfoDAO
}
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext}

trait BitcoinSWalletTest
    extends AsyncFlatSpec
    with BeforeAndAfterAll
    with BitcoinSLogger {
  implicit val actorSystem: ActorSystem = ActorSystem(getClass.getSimpleName)
  implicit val ec: ExecutionContext = actorSystem.dispatcher

  protected lazy val dbConfig: UnitTestDbConfig.type = UnitTestDbConfig
  protected val chainParams: ChainParams = WalletTestUtil.chainParams

  protected val addressDAO: AddressDAO = AddressDAO(dbConfig)
  protected val accountDAO: AccountDAO = AccountDAO(dbConfig)
  protected val mnemonicDAO: MnemonicCodeDAO = MnemonicCodeDAO(dbConfig)
  protected val utxoDAO: UTXOSpendingInfoDAO = UTXOSpendingInfoDAO(dbConfig)

  /** Timeout for async operations */
  protected val timeout: FiniteDuration = 10.seconds

  protected val networkParam: RegTest.type = WalletTestUtil.networkParam

  /* attempt at making a composed withFooDAO, doesn't work :-(
  case class WalletDAOs(
      addressDAO: AddressDAO,
      accountDAO: AccountDAO,
      mnemonicDAO: MnemonicCodeDAO,
      utxoDAO: UTXOSpendingInfoDAO)

  def withDAOs(test: WalletDAOs => Future[Assertion]): Future[Assertion] = {
    // there's got to be a better way
    withAccountDAO(
      accountDAO =>
        withAddressDAO(
          addressDAO =>
            withUtxoDAO(
              utxoDAO =>
                withMnemonicDAO(
                  mnemonicDAO =>
                    test(WalletDAOs(accountDAO = accountDAO,
                                    addressDAO = addressDAO,
                                    mnemonicDAO = mnemonicDAO,
                                    utxoDAO = utxoDAO))))))
  }

  type WithDAO[T <: CRUD[_, _]] = (T => Future[Assertion]) => Future[Assertion]

  val withAddressDAO: WithDAO[AddressDAO] = withDAO(addressDAO, _)

  val withAccountDAO: WithDAO[AccountDAO] = withDAO(accountDAO, _)

  val withMnemonicDAO: WithDAO[MnemonicCodeDAO] = withDAO(mnemonicDAO, _)

  val withUtxoDAO: WithDAO[UTXOSpendingInfoDAO] = withDAO(utxoDAO, _)

  private def withDAO[T <: CRUD[_, _]](
      dao: T,
      test: T => Future[Assertion]): Future[Assertion] =
    WalletDbManagement
      .createTable(dao.table, dbConfig)
      .flatMap(_ => dropTableAfter(dao.table) { test(dao) })

  private def dropTableAfter[T <: Table[T]](table: TableQuery[T])(
      testExecutionF: Future[Assertion]): Future[Assertion] =
    for {
      testResult <- testExecutionF
      _ <- WalletDbManagement.dropTable(table, dbConfig)
    } yield testResult
   */

  override protected def beforeAll(): Unit = {
    Await.result(WalletDbManagement.dropAll(dbConfig), timeout)
    Await.result(WalletDbManagement.createAll(dbConfig), timeout)
  }

  override protected def afterAll(): Unit = {
    // commented out for now, it's handy to inspect DB after tsts has run
    // Await.result(WalletDbManagement.dropAll(dbConfig), timeout)
  }
}
