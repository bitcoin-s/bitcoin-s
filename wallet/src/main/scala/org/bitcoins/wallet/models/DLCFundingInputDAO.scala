package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.Sha256DigestBE
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class DLCFundingInputDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUD[DLCFundingInputDb, TransactionOutPoint] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table: TableQuery[DLCFundingInputsTable] =
    TableQuery[DLCFundingInputsTable]

  override def createAll(
      ts: Vector[DLCFundingInputDb]): Future[Vector[DLCFundingInputDb]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  override protected def findByPrimaryKeys(outPoints: Vector[
    TransactionOutPoint]): Query[Table[_], DLCFundingInputDb, Seq] =
    table.filter(_.outPoint.inSet(outPoints))

  override def findByPrimaryKey(outPoint: TransactionOutPoint): Query[
    Table[_],
    DLCFundingInputDb,
    Seq] = {
    table
      .filter(_.outPoint === outPoint)
  }

  override def findAll(dlcs: Vector[DLCFundingInputDb]): Query[
    Table[_],
    DLCFundingInputDb,
    Seq] =
    findByPrimaryKeys(dlcs.map(_.outPoint))

  def findByEventId(
      eventId: Sha256DigestBE): Future[Vector[DLCFundingInputDb]] = {
    val q = table.filter(_.eventId === eventId)

    database.run(q.result).map(_.toVector)
  }

  def findByEventId(
      eventId: Sha256DigestBE,
      isInitiator: Boolean): Future[Vector[DLCFundingInputDb]] = {
    val q = table
      .filter(_.eventId === eventId)
      .filter(_.isInitiator === isInitiator)

    database.run(q.result).map(_.toVector)
  }
}
