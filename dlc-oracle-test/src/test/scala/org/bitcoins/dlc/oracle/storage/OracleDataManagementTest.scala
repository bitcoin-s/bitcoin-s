package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.dlc.oracle.OracleAnnouncementWithId
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.sorted.OrderedNonces
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.testkit.fixtures.OracleDataManagementFixture

import java.time.Instant
import scala.concurrent.Future

class OracleDataManagementTest extends OracleDataManagementFixture {

  behavior of "OracleDataManagement"

  it must "write and read v1 oracle announcements" in { dataManagement =>
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

    val eventDescriptor = EnumEventDescriptorDLCSubType(
      Vector(
        NormalizedString("a"),
        NormalizedString("b"),
        NormalizedString("c")
      ))
    val oracleEventV1 = OracleEventV1TLV(
      eventDescriptor = eventDescriptor,
      NormalizedString("eventId"),
      FixedOracleEventTimestamp(UInt32(Instant.now().getEpochSecond))
    )

    val announcementSignature =
      OracleAnnouncementV1TLV.buildAnnouncementSignature(
        announcementPrivKey,
        signingVersion = SigningVersion.latest,
        eventTLV = oracleEventV1,
        metadata = metadata
      )

    val announcement = OracleAnnouncementV1TLV(
      announcementSignature,
      eventTLV = oracleEventV1,
      metadata
    )
    val createdF: Future[OracleAnnouncementWithId] =
      dataManagement.createAnnouncement(announcement)

    for {
      annWithId <- createdF
      id = annWithId.id
      readMetadata <- dataManagement.getOracleMetadata(id)
      readAnnouncement <- dataManagement.getAnnouncement(id)
    } yield {
      assert(readMetadata.get.metadata == metadata)
      assert(readAnnouncement.get.announcement == announcement)
    }
  }
}
