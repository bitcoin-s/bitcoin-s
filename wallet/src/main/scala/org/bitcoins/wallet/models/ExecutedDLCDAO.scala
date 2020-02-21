package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.Sha256DigestBE
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class ExecutedDLCDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUD[ExecutedDLCDb, Sha256DigestBE] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table: TableQuery[ExecutedDLCTable] =
    TableQuery[ExecutedDLCTable]

  override def createAll(
      ts: Vector[ExecutedDLCDb]): Future[Vector[ExecutedDLCDb]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256DigestBE]): Query[Table[_], ExecutedDLCDb, Seq] =
    table.filter(_.eventId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256DigestBE): Query[Table[_], ExecutedDLCDb, Seq] = {
    table
      .filter(_.eventId === id)
  }

  override def findAll(
      dlcs: Vector[ExecutedDLCDb]): Query[Table[_], ExecutedDLCDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.eventId))

  def findByEventId(eventId: Sha256DigestBE): Future[Option[ExecutedDLCDb]] = {
    val q = table.filter(_.eventId === eventId)

    database.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[ExecutedDLCDb] =>
        throw new RuntimeException(
          s"More than one DLC per eventId ($eventId), got: $dlcs")
    }
  }
}
