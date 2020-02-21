package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.Sha256DigestBE
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class DLCAcceptDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUD[DLCAcceptDb, Sha256DigestBE] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table: TableQuery[DLCAcceptTable] = TableQuery[DLCAcceptTable]

  override def createAll(ts: Vector[DLCAcceptDb]): Future[Vector[DLCAcceptDb]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256DigestBE]): Query[Table[_], DLCAcceptDb, Seq] =
    table.filter(_.eventId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256DigestBE): Query[Table[_], DLCAcceptDb, Seq] = {
    table
      .filter(_.eventId === id)
  }

  override def findAll(
      dlcs: Vector[DLCAcceptDb]): Query[Table[_], DLCAcceptDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.eventId))

  def findByEventId(eventId: Sha256DigestBE): Future[Option[DLCAcceptDb]] = {
    val q = table.filter(_.eventId === eventId)

    database.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCAcceptDb] =>
        throw new RuntimeException(
          s"More than one DLCAccept per eventId ($eventId), got: $dlcs")
    }
  }
}
