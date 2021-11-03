package org.bitcoins.core.dlc.oracle

import org.bitcoins.core.api.db.DbRowAutoInc
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv.{NormalizedString, OracleMetadata}
import org.bitcoins.crypto.{SchnorrDigitalSignature, SchnorrPublicKey}

case class OracleMetadataDb(
    id: Option[Long],
    announcementPublicKey: SchnorrPublicKey,
    oracleName: NormalizedString,
    oracleDescription: NormalizedString,
    creationTime: UInt32,
    metadataSignature: SchnorrDigitalSignature,
    attestationPublicKey: SchnorrPublicKey,
    attestationPubKeySignature: SchnorrDigitalSignature
) extends DbRowAutoInc[OracleMetadataDb] {

  override def copyWithId(id: Long): OracleMetadataDb = {
    copy(id = Some(id))
  }
}

object OracleMetadataDbHelper {

  def fromOracleMetadata(oracleMetadata: OracleMetadata): OracleMetadataDb = {
    OracleMetadataDb(
      id = None,
      announcementPublicKey = oracleMetadata.announcementPublicKey,
      oracleName = oracleMetadata.oracleName,
      oracleDescription = oracleMetadata.oracleDescription,
      creationTime = oracleMetadata.creationTime,
      metadataSignature = oracleMetadata.metadataSignature.metadataSignature,
      attestationPublicKey = oracleMetadata.attestations.attestationPublicKey,
      attestationPubKeySignature =
        oracleMetadata.attestations.proofOfKnowledge.attestationPubKeySignature
    )
  }
}
