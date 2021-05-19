package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.{ForeignKeyQuery, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCRefundSigDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCRefundSigDb, Sha256Digest]
    with SlickUtil[DLCRefundSigDb, Sha256Digest] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCRefundSigTable] =
    TableQuery[DLCRefundSigTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(
      ts: Vector[DLCRefundSigDb]): Future[Vector[DLCRefundSigDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(ids: Vector[Sha256Digest]): Query[
    DLCRefundSigTable,
    DLCRefundSigDb,
    Seq] =
    table.filter(_.dlcId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256Digest): Query[DLCRefundSigTable, DLCRefundSigDb, Seq] = {
    table
      .filter(_.dlcId === id)
  }

  override def findAll(dlcs: Vector[DLCRefundSigDb]): Query[
    DLCRefundSigTable,
    DLCRefundSigDb,
    Seq] =
    findByPrimaryKeys(dlcs.map(_.dlcId))

  def deleteByDLCId(dlcId: Sha256Digest): Future[Int] = {
    val q = table.filter(_.dlcId === dlcId)
    safeDatabase.run(q.delete)
  }

  def findByDLCId(dlcId: Sha256Digest): Future[Option[DLCRefundSigDb]] = {
    val q = table.filter(_.dlcId === dlcId)

    safeDatabase.runVec(q.result).map(_.headOption)
  }

  class DLCRefundSigTable(tag: Tag)
      extends Table[DLCRefundSigDb](tag, schemaName, "refund_sigs") {

    def dlcId: Rep[Sha256Digest] = column("dlc_id", O.PrimaryKey)

    def acceptSig: Rep[PartialSignature] = column("accept_sig")

    def initiatorSig: Rep[Option[PartialSignature]] = column("initiator_sig")

    def * : ProvenShape[DLCRefundSigDb] =
      (dlcId, acceptSig, initiatorSig).<>(DLCRefundSigDb.tupled,
                                          DLCRefundSigDb.unapply)

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_dlc_id",
                 sourceColumns = dlcId,
                 targetTableQuery = dlcTable)(_.dlcId)
  }
}
