package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.{Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class DLCDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUD[DLCDb, Sha256DigestBE] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table: TableQuery[DLCTable] =
    TableQuery[DLCTable]

  override def createAll(ts: Vector[DLCDb]): Future[Vector[DLCDb]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256DigestBE]): Query[Table[_], DLCDb, Seq] =
    table.filter(_.eventId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256DigestBE): Query[Table[_], DLCDb, Seq] = {
    table
      .filter(_.eventId === id)
  }

  override def findAll(dlcs: Vector[DLCDb]): Query[Table[_], DLCDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.eventId))

  def findByEventId(eventId: Sha256DigestBE): Future[Option[DLCDb]] = {
    val q = table.filter(_.eventId === eventId)

    database.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCDb] =>
        throw new RuntimeException(
          s"More than one DLC per eventId ($eventId), got: $dlcs")
    }
  }

  def findByEventId(eventId: Sha256Digest): Future[Option[DLCDb]] =
    findByEventId(eventId.flip)
}
