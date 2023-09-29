package org.bitcoins.core.dlc.oracle

import org.bitcoins.core.protocol.tlv.{
  OracleAnnouncementV1TLV,
  SchnorrAttestationTLV
}

case class AnnouncementAttestationPair(
    id: Long,
    announcement: OracleAnnouncementV1TLV,
    attestation: SchnorrAttestationTLV) {
  val announcementWithId = OracleAnnouncementWithId(id, announcement)
}
