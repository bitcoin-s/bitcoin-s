package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.protocol.tlv.DLCOutcomeType
import org.bitcoins.crypto.{ECAdaptorSignature, Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCCETSignatureDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCCETSignatureDb, (Sha256DigestBE, DLCOutcomeType)]
    with SlickUtil[DLCCETSignatureDb, (Sha256DigestBE, DLCOutcomeType)] {
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
      DLCOutcomeType)]): Query[DLCCETSignatureTable, DLCCETSignatureDb, Seq] =
    table
      .filter(_.paramHash.inSet(ids.map(_._1)))
      .filter(_.outcome.inSet(ids.map(_._2)))

  override def findByPrimaryKey(id: (Sha256DigestBE, DLCOutcomeType)): Query[
    DLCCETSignatureTable,
    DLCCETSignatureDb,
    Seq] = {
    table
      .filter(_.paramHash === id._1)
      .filter(_.outcome === id._2)
  }

  override def findAll(dlcs: Vector[DLCCETSignatureDb]): Query[
    DLCCETSignatureTable,
    DLCCETSignatureDb,
    Seq] =
    findByPrimaryKeys(dlcs.map(sig => (sig.paramHash, sig.outcome)))

  def findByParamHash(
      paramHash: Sha256DigestBE): Future[Vector[DLCCETSignatureDb]] = {
    val q = table.filter(_.paramHash === paramHash)
    safeDatabase.run(q.result).map(_.toVector)
  }

  def findByParamHash(
      paramHash: Sha256DigestBE,
      isInit: Boolean): Future[Vector[DLCCETSignatureDb]] = {
    val q = table
      .filter(_.paramHash === paramHash)
      .filter(_.isInitiator === isInit)
    safeDatabase.run(q.result).map(_.toVector)
  }

  def findByParamHash(
      paramHash: Sha256Digest): Future[Vector[DLCCETSignatureDb]] =
    findByParamHash(paramHash.flip)

  class DLCCETSignatureTable(tag: Tag)
      extends Table[DLCCETSignatureDb](tag, "wallet_dlc_cet_sigs") {

    def paramHash: Rep[Sha256DigestBE] = column("param_hash")

    def isInitiator: Rep[Boolean] = column("is_initiator")

    def outcome: Rep[DLCOutcomeType] = column("outcome")

    def signature: Rep[ECAdaptorSignature] = column("signature")

    def * : ProvenShape[DLCCETSignatureDb] =
      (paramHash, isInitiator, outcome, signature).<>(DLCCETSignatureDb.tupled,
                                                      DLCCETSignatureDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc_cet_sigs",
                 sourceColumns = (paramHash, isInitiator, outcome))

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_param_hash",
                 sourceColumns = paramHash,
                 targetTableQuery = dlcTable)(_.paramHash)
  }
}
