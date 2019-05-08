package org.bitcoins.wallet.fixtures

import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest._
import slick.jdbc.SQLiteProfile.api._

import scala.language.reflectiveCalls
import scala.concurrent.{Await, Future}

private[fixtures] trait DAOFixture
    extends fixture.AsyncFlatSpec
    with BeforeAndAfterAll
    with BitcoinSWalletTest { this: Suite =>
  import DAOFixture.HasTable

  private[fixtures] val daoAccumulator =
    Vector.newBuilder[HasTable]

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
