package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto.{ECAdaptorSignature, Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCCETSignatureDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCCETSignatureDb, (Sha256Digest, Sha256Digest)]
    with SlickUtil[DLCCETSignatureDb, (Sha256Digest, Sha256Digest)] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCCETSignatureTable] =
    TableQuery[DLCCETSignatureTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(
      ts: Vector[DLCCETSignatureDb]): Future[Vector[DLCCETSignatureDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(ids: Vector[(
      Sha256Digest,
      Sha256Digest)]): Query[DLCCETSignatureTable, DLCCETSignatureDb, Seq] =
    table
      .filter(_.eventId.inSet(ids.map(_._1)))
      .filter(_.outcomeHash.inSet(ids.map(_._2)))

  override def findByPrimaryKey(id: (Sha256Digest, Sha256Digest)): Query[
    DLCCETSignatureTable,
    DLCCETSignatureDb,
    Seq] = {
    table
      .filter(_.eventId === id._1)
      .filter(_.outcomeHash === id._2)
  }

  override def findAll(dlcs: Vector[DLCCETSignatureDb]): Query[
    DLCCETSignatureTable,
    DLCCETSignatureDb,
    Seq] =
    findByPrimaryKeys(dlcs.map(sig => (sig.eventId, sig.outcomeHash)))

  def findByEventId(
      eventId: Sha256Digest): Future[Vector[DLCCETSignatureDb]] = {
    val q = table.filter(_.eventId === eventId)
    safeDatabase.run(q.result).map(_.toVector)
  }

  def findByEventId(
      eventId: Sha256DigestBE): Future[Vector[DLCCETSignatureDb]] =
    findByEventId(eventId.flip)

  class DLCCETSignatureTable(tag: Tag)
      extends Table[DLCCETSignatureDb](tag, "wallet_dlc_cet_sigs") {

    def eventId: Rep[Sha256Digest] = column("event_id")

    def outcomeHash: Rep[Sha256Digest] = column("outcome_hash")

    def signature: Rep[ECAdaptorSignature] = column("signature")

    def * : ProvenShape[DLCCETSignatureDb] =
      (eventId,
       outcomeHash,
       signature) <> (DLCCETSignatureDb.tupled, DLCCETSignatureDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc_cet_sigs",
                 sourceColumns = (eventId, outcomeHash))

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_eventId",
                 sourceColumns = eventId,
                 targetTableQuery = dlcTable)(_.eventId)
  }
}
