package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.{Sha256Digest, Sha256DigestBE}
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class PTLCFundingInputDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUD[PTLCFundingInputDb, TransactionOutPoint] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table: TableQuery[PTLCFundingInputsTable] =
    TableQuery[PTLCFundingInputsTable]

  override def createAll(
      ts: Vector[PTLCFundingInputDb]): Future[Vector[PTLCFundingInputDb]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  override protected def findByPrimaryKeys(outPoints: Vector[
    TransactionOutPoint]): Query[Table[_], PTLCFundingInputDb, Seq] =
    table.filter(_.outPoint.inSet(outPoints))

  override def findByPrimaryKey(outPoint: TransactionOutPoint): Query[
    Table[_],
    PTLCFundingInputDb,
    Seq] = {
    table
      .filter(_.outPoint === outPoint)
  }

  override def findAll(PTLCs: Vector[PTLCFundingInputDb]): Query[
    Table[_],
    PTLCFundingInputDb,
    Seq] =
    findByPrimaryKeys(PTLCs.map(_.outPoint))

  def findByInvoiceId(
      invoiceId: Sha256DigestBE): Future[Vector[PTLCFundingInputDb]] = {
    val q = table.filter(_.invoiceId === invoiceId)

    database.run(q.result).map(_.toVector)
  }

  def findByInvoiceId(
      invoiceId: Sha256Digest): Future[Vector[PTLCFundingInputDb]] =
    findByInvoiceId(invoiceId.flip)
}
