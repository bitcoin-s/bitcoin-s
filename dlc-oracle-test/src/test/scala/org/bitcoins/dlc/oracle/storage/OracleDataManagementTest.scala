package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.dlc.oracle.{NonceSignaturePairDb, OracleMetadataDb}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv.{
  NormalizedString,
  OracleMetadata,
  OracleMetadataSignature,
  SchnorrAttestation
}
import org.bitcoins.core.util.sorted.OrderedNonces
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.testkit.fixtures.OracleDataManagementFixture

import java.time.Instant
import scala.concurrent.Future

class OracleDataManagementTest extends OracleDataManagementFixture {

  behavior of "OracleDataManagement"

  it must "write and read oracle metadata" in { dataManagement =>
    //dataManagement.daos.oracleMetadataDAO.getSql()
    //dataManagement.daos.oracleSchnorrNonceDAO.getSql()
    val announcementPrivKey = ECPrivateKey.freshPrivateKey
    val announcementPubKey = announcementPrivKey.schnorrPublicKey
    val attesatationPubKey = ECPrivateKey.freshPrivateKey.schnorrPublicKey
    val nonces =
      OrderedNonces(Vector.fill(5)(ECPrivateKey.freshPrivateKey.schnorrNonce))
    val name = NormalizedString("oracle_name")
    val description = NormalizedString("oracle_description")
    val creationTime: UInt32 = UInt32(Instant.now.getEpochSecond)
    val schnorrAttestation: SchnorrAttestation =
      SchnorrAttestation.build(announcementPrivKey, attesatationPubKey, nonces)
    val metadataSignature: OracleMetadataSignature = OracleMetadataSignature
      .buildSignature(announcementPrivKey = announcementPrivKey,
                      oracleName = name,
                      oracleDescription = description,
                      creationTime = creationTime,
                      schnorrAttestation = schnorrAttestation)

    val metadata = OracleMetadata(announcementPubKey,
                                  name,
                                  description,
                                  creationTime = creationTime,
                                  attestations = schnorrAttestation,
                                  metadataSignature = metadataSignature)

    val createdF: Future[(OracleMetadataDb, Vector[NonceSignaturePairDb])] =
      dataManagement.createOracleMetadata(metadata)

    for {
      (metadataDb, _) <- createdF
      id = metadataDb.id
      read <- dataManagement.getOracleMetadata(id.get)
    } yield {
      assert(read.get.metadata == metadata)
    }
  }
}
