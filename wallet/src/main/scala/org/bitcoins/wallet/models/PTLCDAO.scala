package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.{Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class PTLCDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUD[PTLCDb, Sha256DigestBE] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table: TableQuery[PTLCTable] = TableQuery[PTLCTable]

  override def createAll(ts: Vector[PTLCDb]): Future[Vector[PTLCDb]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256DigestBE]): Query[Table[_], PTLCDb, Seq] =
    table.filter(_.invoiceId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256DigestBE): Query[Table[_], PTLCDb, Seq] = {
    table
      .filter(_.invoiceId === id)
  }

  override def findAll(dlcs: Vector[PTLCDb]): Query[Table[_], PTLCDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.invoiceId))

  def findByInvoiceId(invoiceId: Sha256DigestBE): Future[Option[PTLCDb]] = {
    val q = table.filter(_.invoiceId === invoiceId)

    database.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case ptlcs: Vector[PTLCDb] =>
        throw new RuntimeException(
          s"More than one PTLC per invoiceId ($invoiceId), got: $ptlcs")
    }
  }

  def findByInvoiceId(invoiceId: Sha256Digest): Future[Option[PTLCDb]] =
    findByInvoiceId(invoiceId.flip)
}
