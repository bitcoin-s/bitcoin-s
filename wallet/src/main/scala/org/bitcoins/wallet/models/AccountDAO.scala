package org.bitcoins.wallet.models

import org.bitcoins.core.hd._
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class AccountDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUD[AccountDb, (HDCoin, Int)] {

  import org.bitcoins.db.DbCommonsColumnMappers._

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

  def findByAccount(account: HDAccount): Future[Option[AccountDb]] = {
    val q = table
      .filter(_.coinType === account.coin.coinType)
      .filter(_.purpose === account.purpose)
      .filter(_.index === account.index)

    database.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case accounts: Vector[AccountDb] =>
        //yikes, we should not have more the one account per coin type/purpose
        throw new RuntimeException(
          s"More than one account per account=${account}, got=${accounts}")
    }
  }
}
