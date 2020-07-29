package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCRefundSigDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCRefundSigDb, (Sha256DigestBE, Boolean)]
    with SlickUtil[DLCRefundSigDb, (Sha256DigestBE, Boolean)] {
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

  override protected def findByPrimaryKeys(ids: Vector[
    (Sha256DigestBE, Boolean)]): Query[DLCRefundSigTable, DLCRefundSigDb, Seq] =
    table.filter(_.paramHash.inSet(ids.map(_._1)))

  override def findByPrimaryKey(id: (Sha256DigestBE, Boolean)): Query[
    DLCRefundSigTable,
    DLCRefundSigDb,
    Seq] = {
    val (paramHash, isInit) = id
    table
      .filter(_.paramHash === paramHash)
      .filter(_.isInitiator === isInit)
  }

  override def findAll(dlcs: Vector[DLCRefundSigDb]): Query[
    DLCRefundSigTable,
    DLCRefundSigDb,
    Seq] =
    findByPrimaryKeys(dlcs.map(dlc => (dlc.paramHash, dlc.isInitiator)))

  def deleteByParamHash(paramHash: Sha256DigestBE): Future[Int] = {
    val q = table.filter(_.paramHash === paramHash)
    safeDatabase.run(q.delete)
  }

  def findByParamHash(
      paramHash: Sha256DigestBE): Future[Vector[DLCRefundSigDb]] = {
    val q = table.filter(_.paramHash === paramHash)

    safeDatabase.runVec(q.result)
  }

  def findByParamHash(
      paramHash: Sha256DigestBE,
      isInit: Boolean): Future[Option[DLCRefundSigDb]] = {
    val q = table
      .filter(_.paramHash === paramHash)
      .filter(_.isInitiator === isInit)

    safeDatabase.runVec(q.result).map(_.headOption)
  }

  def findByParamHash(paramHash: Sha256Digest): Future[Vector[DLCRefundSigDb]] =
    findByParamHash(paramHash.flip)

  class DLCRefundSigTable(tag: Tag)
      extends Table[DLCRefundSigDb](tag, "wallet_dlc_refund_sigs") {

    def paramHash: Rep[Sha256DigestBE] = column("param_hash")

    def isInitiator: Rep[Boolean] = column("is_initiator")

    def refundSig: Rep[PartialSignature] = column("refund_sig")

    def * : ProvenShape[DLCRefundSigDb] =
      (paramHash, isInitiator, refundSig).<>(DLCRefundSigDb.tupled,
                                             DLCRefundSigDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc_refund_sig",
                 sourceColumns = (paramHash, isInitiator))

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_param_hash",
                 sourceColumns = paramHash,
                 targetTableQuery = dlcTable)(_.paramHash)
  }
}
