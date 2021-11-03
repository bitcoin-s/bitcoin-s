package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.dlc.oracle.OracleMetadataDb
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv.{NormalizedString}
import org.bitcoins.crypto.{SchnorrDigitalSignature, SchnorrPublicKey}
import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import slick.lifted.ProvenShape

import scala.concurrent.ExecutionContext

case class OracleMetadataDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCOracleAppConfig)
    extends CRUDAutoInc[OracleMetadataDb] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[OracleMetadataTable] =
    TableQuery[OracleMetadataTable]

  def findByAttestationPubKeyAction(
      attestationPubKey: SchnorrPublicKey): DBIOAction[
    Option[OracleMetadataDb],
    NoStream,
    Effect.Read] = {
    table
      .filter(_.attestationPubKey === attestationPubKey)
      .result
      .map(_.headOption)
  }

  class OracleMetadataTable(tag: Tag)
      extends TableAutoInc[OracleMetadataDb](tag,
                                             schemaName = schemaName,
                                             tableName = "oracle_metadata") {

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
      (id.?,
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
