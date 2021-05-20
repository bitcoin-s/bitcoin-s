package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted._

import scala.concurrent.{ExecutionContext, Future}

case class DLCCETSignaturesDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCCETSignaturesDb, (Sha256Digest, Long)]
    with SlickUtil[DLCCETSignaturesDb, (Sha256Digest, Long)] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCCETSignatureTable] =
    TableQuery[DLCCETSignatureTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(
      ts: Vector[DLCCETSignaturesDb]): Future[Vector[DLCCETSignaturesDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[(Sha256Digest, Long)]): Query[
    DLCCETSignatureTable,
    DLCCETSignaturesDb,
    Seq] = {
    // is there a better way to do this?
    val starting = table.filter(_.dlcId =!= Sha256Digest.empty)

    val group = ids.groupBy(_._1)

    group
      .foldLeft(starting) { case (accum, (dlcId, vec)) =>
        accum.flatMap { _ =>
          table.filter(t => t.dlcId === dlcId && t.index.inSet(vec.map(_._2)))
        }
      }
  }

  override def findByPrimaryKey(id: (Sha256Digest, Long)): Query[
    DLCCETSignatureTable,
    DLCCETSignaturesDb,
    Seq] = {
    table.filter(t => t.dlcId === id._1 && t.index === id._2)
  }

  override def find(t: DLCCETSignaturesDb): Query[
    DLCCETSignatureTable,
    DLCCETSignaturesDb,
    Seq] = {
    findByPrimaryKey((t.dlcId, t.index))
  }

  override def findAll(dlcs: Vector[DLCCETSignaturesDb]): Query[
    DLCCETSignatureTable,
    DLCCETSignaturesDb,
    Seq] =
    findByPrimaryKeys(dlcs.map(sig => (sig.dlcId, sig.index)))

  def findByDLCId(dlcId: Sha256Digest): Future[Vector[DLCCETSignaturesDb]] = {
    val q = table.filter(_.dlcId === dlcId)
    safeDatabase.runVec(q.result)
  }

  def deleteByDLCId(dlcId: Sha256Digest): Future[Int] = {
    val q = table.filter(_.dlcId === dlcId)
    safeDatabase.run(q.delete)
  }

  class DLCCETSignatureTable(tag: Tag)
      extends Table[DLCCETSignaturesDb](tag, schemaName, "cet_sigs") {

    def dlcId: Rep[Sha256Digest] = column("dlc_id")

    def index: Rep[Long] = column("index")

    def sigPoint: Rep[ECPublicKey] = column("sig_point")

    def accepterSig: Rep[ECAdaptorSignature] = column("accepter_sig")

    def initiatorSig: Rep[Option[ECAdaptorSignature]] = column("initiator_sig")

    def * : ProvenShape[DLCCETSignaturesDb] =
      (dlcId, index, sigPoint, accepterSig, initiatorSig).<>(
        DLCCETSignaturesDb.tupled,
        DLCCETSignaturesDb.unapply)

    def pk: PrimaryKey =
      primaryKey(name = "pk_cet_sigs", sourceColumns = (dlcId, index))

    def fk =
      foreignKey("fk_dlc_id",
                 sourceColumns = dlcId,
                 targetTableQuery = dlcTable)(_.dlcId)
  }
}
