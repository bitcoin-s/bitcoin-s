package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.{Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class DLCOfferDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUD[DLCOfferDb, Sha256DigestBE] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table: TableQuery[DLCOfferTable] = TableQuery[DLCOfferTable]

  override def createAll(ts: Vector[DLCOfferDb]): Future[Vector[DLCOfferDb]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256DigestBE]): Query[Table[_], DLCOfferDb, Seq] =
    table.filter(_.eventId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256DigestBE): Query[Table[_], DLCOfferDb, Seq] = {
    table
      .filter(_.eventId === id)
  }

  override def findAll(
      dlcs: Vector[DLCOfferDb]): Query[Table[_], DLCOfferDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.eventId))

  def findByEventId(eventId: Sha256DigestBE): Future[Option[DLCOfferDb]] = {
    val q = table.filter(_.eventId === eventId)

    database.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCOfferDb] =>
        throw new RuntimeException(
          s"More than one DLCOffer per eventId ($eventId), got: $dlcs")
    }
  }

  def findByEventId(eventId: Sha256Digest): Future[Option[DLCOfferDb]] =
    findByEventId(eventId.flip)
}
