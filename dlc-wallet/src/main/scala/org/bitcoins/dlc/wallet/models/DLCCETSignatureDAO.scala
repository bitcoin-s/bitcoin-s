package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted._

import scala.concurrent.{ExecutionContext, Future}

case class DLCCETSignaturePrimaryKey(
    dlcId: Sha256Digest,
    contractIndex: Long,
    isInitiator: Boolean)

case class DLCCETSignatureDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCCETSignatureDb, DLCCETSignaturePrimaryKey]
    with SlickUtil[DLCCETSignatureDb, DLCCETSignaturePrimaryKey] {
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

  override protected def findByPrimaryKeys(
      ids: Vector[DLCCETSignaturePrimaryKey]): Query[
    DLCCETSignatureTable,
    DLCCETSignatureDb,
    Seq] = {
    // is there a better way to do this?
    val starting = table.filter(_.dlcId =!= Sha256Digest.empty)

    val group = ids.groupBy(i => (i.dlcId, i.isInitiator))

    group
      .foldLeft(starting) { case (accum, ((dlcId, isInit), vec)) =>
        accum.flatMap { _ =>
          table.filter(t =>
            t.dlcId === dlcId && t.isInitiator === isInit && t.index.inSet(
              vec.map(_.contractIndex)))
        }
      }
  }

  override def findByPrimaryKey(id: DLCCETSignaturePrimaryKey): Query[
    DLCCETSignatureTable,
    DLCCETSignatureDb,
    Seq] = {
    table.filter(t =>
      t.dlcId === id.dlcId && t.index === id.contractIndex && id.isInitiator)
  }

  override def find(t: DLCCETSignatureDb): Query[
    DLCCETSignatureTable,
    DLCCETSignatureDb,
    Seq] = {
    findByPrimaryKey(DLCCETSignaturePrimaryKey(t.dlcId, t.index, t.isInitiator))
  }

  override def findAll(dlcs: Vector[DLCCETSignatureDb]): Query[
    DLCCETSignatureTable,
    DLCCETSignatureDb,
    Seq] =
    findByPrimaryKeys(dlcs.map(sig =>
      DLCCETSignaturePrimaryKey(sig.dlcId, sig.index, sig.isInitiator)))

  def findByDLCId(dlcId: Sha256Digest): Future[Vector[DLCCETSignatureDb]] = {
    val q = table.filter(_.dlcId === dlcId)
    safeDatabase.runVec(q.result)
  }

  def findByDLCId(
      dlcId: Sha256Digest,
      isInitiator: Boolean): Future[Vector[DLCCETSignatureDb]] = {
    val q =
      table.filter(t => t.dlcId === dlcId && t.isInitiator === isInitiator)
    safeDatabase.runVec(q.result)
  }

  def deleteByDLCId(dlcId: Sha256Digest): Future[Int] = {
    val q = table.filter(_.dlcId === dlcId)
    safeDatabase.run(q.delete)
  }

  class DLCCETSignatureTable(tag: Tag)
      extends Table[DLCCETSignatureDb](tag, schemaName, "cet_sigs") {

    def dlcId: Rep[Sha256Digest] = column("dlc_id")

    def index: Rep[Long] = column("index")

    def isInitiator: Rep[Boolean] = column("is_initiator")

    def sigPoint: Rep[ECPublicKey] = column("sig_point")

    def adaptorSig: Rep[ECAdaptorSignature] = column("adaptor_sig")

    def * : ProvenShape[DLCCETSignatureDb] =
      (dlcId, index, isInitiator, sigPoint, adaptorSig).<>(
        DLCCETSignatureDb.tupled,
        DLCCETSignatureDb.unapply)

    def pk: PrimaryKey =
      primaryKey(name = "pk_cet_sigs",
                 sourceColumns = (dlcId, index, isInitiator))

    def fk =
      foreignKey("fk_dlc_id",
                 sourceColumns = dlcId,
                 targetTableQuery = dlcTable)(_.dlcId)
  }
}
