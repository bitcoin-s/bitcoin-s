package org.bitcoins.explorer.model

import org.bitcoins.core.protocol.tlv.{
  OracleAnnouncementV0TLV,
  OracleAttestmentV0TLV
}
import org.bitcoins.crypto.Sha256Digest

case class SbAnnouncementEvent(
    id: Sha256Digest,
    oracleName: String,
    description: String,
    uri: Option[String],
    announcement: OracleAnnouncementV0TLV,
    attestations: Option[OracleAttestmentV0TLV],
    outcome: Option[String])
