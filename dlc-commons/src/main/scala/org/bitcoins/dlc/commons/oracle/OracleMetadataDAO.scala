package org.bitcoins.dlc.commons.oracle

import org.bitcoins.core.dlc.oracle.OracleMetadataDb
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv.NormalizedString
import org.bitcoins.crypto.{SchnorrDigitalSignature, SchnorrPublicKey}
import org.bitcoins.db.{CRUD, DbAppConfig, SlickUtil}
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class OracleMetadataDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DbAppConfig)
    extends CRUD[OracleMetadataDb, Long]
    with SlickUtil[OracleMetadataDb, Long] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)

  import mappers._
  import profile.api._

  override val table: TableQuery[OracleMetadataTable] =
    TableQuery[OracleMetadataTable]

  override def createAll(
      ts: Vector[OracleMetadataDb]): Future[Vector[OracleMetadataDb]] = {
    createAllNoAutoInc(ts, safeDatabase)
  }

  override def findByPrimaryKeys(ids: Vector[Long]): profile.api.Query[
    profile.api.Table[OracleMetadataDb],
    OracleMetadataDb,
    Seq] = {
    table.filter(_.announcementId.inSet(ids))
  }

  override def findAll(ts: Vector[OracleMetadataDb]): profile.api.Query[
    profile.api.Table[OracleMetadataDb],
    OracleMetadataDb,
    Seq] = {
    findByPrimaryKeys(ts.map(_.announcementId))
  }

  def findByAttestationPubKeyAction(
      attestationPubKey: SchnorrPublicKey): DBIOAction[
    Vector[OracleMetadataDb],
    NoStream,
    Effect.Read] = {
    table
      .filter(_.attestationPubKey === attestationPubKey)
      .result
      .map(_.toVector)
  }

  class OracleMetadataTable(tag: Tag)
      extends Table[OracleMetadataDb](tag, schemaName, "oracle_metadata") {

    def announcementId: Rep[Long] = column("id", O.Unique)
    def publicKey: Rep[SchnorrPublicKey] = column("public_key")

    def oracleName: Rep[NormalizedString] = column("oracle_name")

    def oracleDescription: Rep[NormalizedString] = column("oracle_description")

    def creationTime: Rep[UInt32] = column("creation_time")

    def metadataSignature: Rep[SchnorrDigitalSignature] =
      column("oracle_metadata_signature")

    def attestationPubKey: Rep[SchnorrPublicKey] = column(
      "attestation_public_key")

    def attestationPubKeySignature: Rep[SchnorrDigitalSignature] = column(
      "attestation_public_key_signature")

    override def * : ProvenShape[OracleMetadataDb] = {
      (announcementId,
       publicKey,
       oracleName,
       oracleDescription,
       creationTime,
       metadataSignature,
       attestationPubKey,
       attestationPubKeySignature)
        .<>(OracleMetadataDb.tupled, OracleMetadataDb.unapply)
    }
  }
}
