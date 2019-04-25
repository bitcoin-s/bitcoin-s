package org.bitcoins.wallet.models

import org.bitcoins.core.hd._
import org.bitcoins.wallet.db.WalletDbConfig
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.db.CRUD
import org.bitcoins.db.SlickUtil

case class AccountDAO(dbConfig: WalletDbConfig)(
    implicit executionContext: ExecutionContext)
    extends CRUD[AccountDb, (HDCoinType, Int)] {

  import org.bitcoins.db.DbCommonsColumnMappers._

  override val ec: ExecutionContext = executionContext

  override val table: TableQuery[AccountTable] = TableQuery[AccountTable]

  override def createAll(ts: Vector[AccountDb]): Future[Vector[AccountDb]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  override protected def findByPrimaryKeys(
      ids: Vector[(HDCoinType, Int)]): Query[Table[_], AccountDb, Seq] = ???

  override def findByPrimaryKey(
      id: (HDCoinType, Int)): Query[Table[_], AccountDb, Seq] = {
    val (coin, index) = id
    table
      .filter(_.coin === coin)
      .filter(_.index === index)
  }

  override def findAll(
      accounts: Vector[AccountDb]): Query[Table[_], AccountDb, Seq] =
    findByPrimaryKeys(
      accounts.map(acc => (acc.hdAccount.coin.coinType, acc.hdAccount.index)))

  def findAll(): Future[Vector[AccountDb]] = {
    val query = table.result
    database.run(query).map(_.toVector)
  }
}
