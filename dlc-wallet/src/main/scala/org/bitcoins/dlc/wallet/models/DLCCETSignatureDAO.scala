package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCCETSignatureDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCCETSignatureDb, (Sha256DigestBE, ECPublicKey)]
    with SlickUtil[DLCCETSignatureDb, (Sha256DigestBE, ECPublicKey)] {
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
      Sha256DigestBE,
      ECPublicKey)]): Query[DLCCETSignatureTable, DLCCETSignatureDb, Seq] =
    table
      .filter(_.paramHash.inSet(ids.map(_._1)))
      .filter(_.sigPoint.inSet(ids.map(_._2)))

  override def findByPrimaryKey(id: (Sha256DigestBE, ECPublicKey)): Query[
    DLCCETSignatureTable,
    DLCCETSignatureDb,
    Seq] = {
    table
      .filter(_.paramHash === id._1)
      .filter(_.sigPoint === id._2)
  }

  override def findAll(dlcs: Vector[DLCCETSignatureDb]): Query[
    DLCCETSignatureTable,
    DLCCETSignatureDb,
    Seq] =
    findByPrimaryKeys(dlcs.map(sig => (sig.paramHash, sig.sigPoint)))

  def findByParamHash(
      paramHash: Sha256DigestBE): Future[Vector[DLCCETSignatureDb]] = {
    val q = table.filter(_.paramHash === paramHash)
    safeDatabase.runVec(q.result)
  }

  def findByParamHash(
      paramHash: Sha256DigestBE,
      isInit: Boolean): Future[Vector[DLCCETSignatureDb]] = {
    val q = table
      .filter(_.paramHash === paramHash)
      .filter(_.isInitiator === isInit)
    safeDatabase.runVec(q.result)
  }

  def findByParamHash(
      paramHash: Sha256Digest): Future[Vector[DLCCETSignatureDb]] =
    findByParamHash(paramHash.flip)

  def deleteByParamHash(paramHash: Sha256DigestBE): Future[Int] = {
    val q = table.filter(_.paramHash === paramHash)
    safeDatabase.run(q.delete)
  }

  class DLCCETSignatureTable(tag: Tag)
      extends Table[DLCCETSignatureDb](tag, "wallet_dlc_cet_sigs") {

    def paramHash: Rep[Sha256DigestBE] = column("param_hash")

    def isInitiator: Rep[Boolean] = column("is_initiator")

    def sigPoint: Rep[ECPublicKey] = column("sig_point")

    def signature: Rep[ECAdaptorSignature] = column("signature")

    def * : ProvenShape[DLCCETSignatureDb] =
      (paramHash, isInitiator, sigPoint, signature).<>(
        DLCCETSignatureDb.tupled,
        DLCCETSignatureDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc_cet_sigs",
                 sourceColumns = (paramHash, isInitiator, sigPoint))

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_param_hash",
                 sourceColumns = paramHash,
                 targetTableQuery = dlcTable)(_.paramHash)
  }
}
