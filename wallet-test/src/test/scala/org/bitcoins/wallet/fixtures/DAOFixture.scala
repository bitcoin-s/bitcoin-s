package org.bitcoins.wallet.fixtures

import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest._
import slick.jdbc.SQLiteProfile.api._

import scala.language.reflectiveCalls
import scala.concurrent.{Await, Future}
import org.bitcoins.wallet.config.WalletAppConfig

private[fixtures] trait DAOFixture
    extends fixture.AsyncFlatSpec
    with BeforeAndAfterAll
    with BitcoinSWalletTest { this: Suite =>
  import DAOFixture.HasTable

  private[fixtures] val daoAccumulator =
    Vector.newBuilder[HasTable]

  // to get around the config in `BitcoinSWalletTest` not resolving
  // as an AppConfig
  private implicit val walletConfig: WalletAppConfig = config.walletConf

  override def beforeAll(): Unit = {
    val tables = daoAccumulator.result()

    val dropTablesF =
      Future.sequence(
        tables.map((dao: HasTable) => WalletDbManagement.dropTable(dao.table)))

    val createTablesF =
      dropTablesF.flatMap { _ =>
        Future.sequence(tables.map((dao: HasTable) =>
          WalletDbManagement.createTable(dao.table)))
      }

    Await.result(createTablesF, timeout)

    super.beforeAll()
  }

}
private[fixtures] object DAOFixture {

  type HasTable = { def table[T <: Table[_]]: TableQuery[T] }
}
