package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.{Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class PTLCAcceptDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUD[PTLCAcceptDb, Sha256DigestBE] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table: TableQuery[PTLCAcceptTable] =
    TableQuery[PTLCAcceptTable]

  override def createAll(
      ts: Vector[PTLCAcceptDb]): Future[Vector[PTLCAcceptDb]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256DigestBE]): Query[Table[_], PTLCAcceptDb, Seq] =
    table.filter(_.invoiceId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256DigestBE): Query[Table[_], PTLCAcceptDb, Seq] = {
    table
      .filter(_.invoiceId === id)
  }

  override def findAll(
      dlcs: Vector[PTLCAcceptDb]): Query[Table[_], PTLCAcceptDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.invoiceId))

  def findByInvoiceId(
      invoiceId: Sha256DigestBE): Future[Option[PTLCAcceptDb]] = {
    val q = table.filter(_.invoiceId === invoiceId)

    database.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case ptlcs: Vector[PTLCAcceptDb] =>
        throw new RuntimeException(
          s"More than one DLCOffer per invoiceId ($invoiceId), got: $ptlcs")
    }
  }

  def findByInvoiceId(invoiceId: Sha256Digest): Future[Option[PTLCAcceptDb]] =
    findByInvoiceId(invoiceId.flip)
}
