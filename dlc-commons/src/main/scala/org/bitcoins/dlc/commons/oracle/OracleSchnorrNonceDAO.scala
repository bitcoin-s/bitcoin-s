package org.bitcoins.dlc.commons.oracle

import org.bitcoins.core.dlc.oracle.{NonceSignaturePairDb, OracleMetadataDb}
import org.bitcoins.crypto.{FieldElement, SchnorrDigitalSignature, SchnorrNonce}
import org.bitcoins.db.{CRUD, DbAppConfig, SlickUtil}
import slick.lifted.{ForeignKeyQuery, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class OracleSchnorrNonceDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DbAppConfig)
    extends CRUD[NonceSignaturePairDb, Long]
    with SlickUtil[NonceSignaturePairDb, Long] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)

  import mappers._
  import profile.api._

  override val table: TableQuery[NonceTable] = TableQuery[NonceTable]

  private val metadataDAO: OracleMetadataDAO = OracleMetadataDAO()
  private val metadataTable = metadataDAO.table

  override def createAll(ts: Vector[NonceSignaturePairDb]): Future[
    Vector[NonceSignaturePairDb]] = {
    createAllNoAutoInc(ts, safeDatabase)
  }

  override def findByPrimaryKeys(ids: Vector[Long]): profile.api.Query[
    profile.api.Table[NonceSignaturePairDb],
    NonceSignaturePairDb,
    Seq] = {
    table.filter(_.announcementId.inSet(ids))
  }

  override def findAll(ts: Vector[NonceSignaturePairDb]): profile.api.Query[
    profile.api.Table[NonceSignaturePairDb],
    NonceSignaturePairDb,
    Seq] = {
    findByPrimaryKeys(ts.map(_.announcementId))
  }

  def findByIdsAction(ids: Vector[Long]): DBIOAction[
    Vector[NonceSignaturePairDb],
    NoStream,
    Effect.Read] = {
    findByPrimaryKeys(ids).result
      .map(_.toVector)
  }

  def findByIds(ids: Vector[Long]): Future[Vector[NonceSignaturePairDb]] = {
    val action = findByIdsAction(ids)
    safeDatabase.run(action)
  }

  def findByIdAction(id: Long): DBIOAction[
    Vector[NonceSignaturePairDb],
    NoStream,
    Effect.Read] = {
    table
      .filter(_.announcementId === id)
      .result
      .map(_.toVector)
  }

  def findByNonceAction(nonce: SchnorrNonce): DBIOAction[
    Option[NonceSignaturePairDb],
    NoStream,
    Effect.Read] = {
    table.filter(_.nonce === nonce).result.map(_.headOption)
  }

  def findByNoncesAction(nonces: Vector[SchnorrNonce]): DBIOAction[
    Vector[NonceSignaturePairDb],
    NoStream,
    Effect.Read] = {
    table
      .filter(_.nonce.inSet(nonces))
      .result
      .map(_.toVector)
  }

  def findByNonces(
      nonces: Vector[SchnorrNonce]): Future[Vector[NonceSignaturePairDb]] = {
    val action = findByNoncesAction(nonces)
    safeDatabase.run(action)
  }

  def findByNonce(nonce: SchnorrNonce): Future[Option[NonceSignaturePairDb]] = {
    safeDatabase.run(findByNonceAction(nonce))
  }

  /** Updates the outcome/attestation on a nonce signature pair in the db */
  def updateNonceSignatureDb(
      nonceSignatureDb: NonceSignaturePairDb): DBIOAction[
    Int,
    NoStream,
    Effect.Write] = {
//    require(nonceSignatureDb.attestationOpt.isDefined)
    table
      .filter(_.nonce === nonceSignatureDb.nonce)
      .update(nonceSignatureDb)
  }

  def deleteByAnnouncementId(
      announcementId: Long): DBIOAction[Int, NoStream, Effect.Write] = {
    table.filter(_.announcementId === announcementId).delete
  }

  class NonceTable(tag: Tag)
      extends Table[NonceSignaturePairDb](tag,
                                          schemaName,
                                          "oracle_schnorr_nonces") {

    def announcementId: Rep[Long] = column("id")

    def nonce: Rep[SchnorrNonce] = column("nonce", O.Unique)

    def attestationOpt: Rep[Option[FieldElement]] = column("attestation")

    def nonceProof: Rep[SchnorrDigitalSignature] =
      column("nonce_proof", O.Unique)

    def outcomeOpt: Rep[Option[String]] = column("outcome")

    override def * : ProvenShape[NonceSignaturePairDb] = {
      (announcementId, nonce, nonceProof, attestationOpt, outcomeOpt)
        .<>(NonceSignaturePairDb.tupled, NonceSignaturePairDb.unapply)
    }

    def fkMetadataId: ForeignKeyQuery[_, OracleMetadataDb] = {
      foreignKey("fk_metadata_id",
                 sourceColumns = announcementId,
                 targetTableQuery = metadataTable)(_.announcementId)
    }
  }
}
