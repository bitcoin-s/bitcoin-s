package org.bitcoins.wallet.models

import org.bitcoins.core.hd._
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{Future}
import org.bitcoins.db.CRUD
import org.bitcoins.db.SlickUtil
import org.bitcoins.db.AppConfig
import scala.concurrent.ExecutionContext

case class AccountDAO()(implicit val ec: ExecutionContext)
    extends CRUD[AccountDb, (HDCoin, Int)] {

  import org.bitcoins.db.DbCommonsColumnMappers._

  override def appConfig: WalletAppConfig = WalletAppConfig()

  override val table: TableQuery[AccountTable] = TableQuery[AccountTable]

  override def createAll(ts: Vector[AccountDb]): Future[Vector[AccountDb]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  override protected def findByPrimaryKeys(
      ids: Vector[(HDCoin, Int)]): Query[Table[_], AccountDb, Seq] = ???

  override def findByPrimaryKey(
      id: (HDCoin, Int)): Query[Table[_], AccountDb, Seq] = {
    val (coin, index) = id
    table
      .filter(_.coinType === coin.coinType)
      .filter(_.purpose === coin.purpose)
      .filter(_.index === index)
  }

  override def findAll(
      accounts: Vector[AccountDb]): Query[Table[_], AccountDb, Seq] =
    findByPrimaryKeys(
      accounts.map(acc => (acc.hdAccount.coin, acc.hdAccount.index)))

  def findAll(): Future[Vector[AccountDb]] = {
    val query = table.result
    database.run(query).map(_.toVector)
  }
}
