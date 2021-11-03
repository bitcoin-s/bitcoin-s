package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.dlc.oracle.{NonceSignaturePairDb, OracleMetadataDb}
import org.bitcoins.crypto.{SchnorrDigitalSignature, SchnorrNonce}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import slick.lifted.{ForeignKeyQuery, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class OracleSchnorrNonceDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCOracleAppConfig)
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
    table.filter(_.id.inSet(ids))
  }

  override def findAll(ts: Vector[NonceSignaturePairDb]): profile.api.Query[
    profile.api.Table[NonceSignaturePairDb],
    NonceSignaturePairDb,
    Seq] = {
    findByPrimaryKeys(ts.map(_.id))
  }

  def findByIdAction(id: Long): DBIOAction[
    Vector[NonceSignaturePairDb],
    NoStream,
    Effect.Read] = {
    table
      .filter(_.id === id)
      .result
      .map(_.toVector)
  }

  class NonceTable(tag: Tag)
      extends Table[NonceSignaturePairDb](tag,
                                          schemaName,
                                          "oracle_schnorr_nonces") {

    def id: Rep[Long] = column("id")
    def nonce: Rep[SchnorrNonce] = column("nonce", O.Unique)

    def signature: Rep[SchnorrDigitalSignature] = column("signature", O.Unique)

    override def * : ProvenShape[NonceSignaturePairDb] = {
      (id, nonce, signature)
        .<>(NonceSignaturePairDb.tupled, NonceSignaturePairDb.unapply)
    }

    def fkMetadataId: ForeignKeyQuery[_, OracleMetadataDb] = {
      foreignKey("fk_metadata_id",
                 sourceColumns = id,
                 targetTableQuery = metadataTable)(_.id)
    }
  }
}
