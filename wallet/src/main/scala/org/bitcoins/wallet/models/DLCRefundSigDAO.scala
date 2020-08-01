package org.bitcoins.wallet.models

import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCRefundSigDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
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
    table.filter(_.eventId.inSet(ids.map(_._1)))

  override def findByPrimaryKey(id: (Sha256DigestBE, Boolean)): Query[
    DLCRefundSigTable,
    DLCRefundSigDb,
    Seq] = {
    val (eventId, isInit) = id
    table
      .filter(_.eventId === eventId)
      .filter(_.isInitiator === isInit)
  }

  override def findAll(dlcs: Vector[DLCRefundSigDb]): Query[
    DLCRefundSigTable,
    DLCRefundSigDb,
    Seq] =
    findByPrimaryKeys(dlcs.map(dlc => (dlc.eventId, dlc.isInitiator)))

  def findByEventId(eventId: Sha256DigestBE): Future[Vector[DLCRefundSigDb]] = {
    val q = table.filter(_.eventId === eventId)

    safeDatabase.runVec(q.result)
  }

  def findByEventId(eventId: Sha256Digest): Future[Vector[DLCRefundSigDb]] =
    findByEventId(eventId.flip)

  class DLCRefundSigTable(tag: Tag)
      extends Table[DLCRefundSigDb](tag, "wallet_dlc_refund_sigs") {

    def eventId: Rep[Sha256DigestBE] = column("event_id")

    def isInitiator: Rep[Boolean] = column("is_initiator")

    def refundSig: Rep[PartialSignature] = column("refund_sig")

    def * : ProvenShape[DLCRefundSigDb] =
      (eventId,
       isInitiator,
       refundSig) <> (DLCRefundSigDb.tupled, DLCRefundSigDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc_refund_sig",
                 sourceColumns = (eventId, isInitiator))

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_eventId",
                 sourceColumns = eventId,
                 targetTableQuery = dlcTable)(_.eventId)
  }
}
