package org.bitcoins.core.dlc.oracle

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv.{NormalizedString, OracleMetadata}
import org.bitcoins.crypto.{SchnorrDigitalSignature, SchnorrPublicKey}

case class OracleMetadataDb(
    announcementId: Long,
    announcementPublicKey: SchnorrPublicKey,
    oracleName: NormalizedString,
    oracleDescription: NormalizedString,
    creationTime: UInt32,
    metadataSignature: SchnorrDigitalSignature,
    attestationPublicKey: SchnorrPublicKey,
    attestationPubKeySignature: SchnorrDigitalSignature
)

object OracleMetadataDbHelper {

  def fromOracleMetadata(
      oracleMetadata: OracleMetadata,
      announcementId: Long): OracleMetadataDb = {
    OracleMetadataDb(
      announcementId = announcementId,
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
