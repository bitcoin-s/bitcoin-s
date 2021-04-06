package org.bitcoins.explorer.model

import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto.Sha256Digest

case class CreateAttestations(
    announcementHash: Sha256Digest,
    attestment: OracleAttestmentV0TLV) {

  override def toString: String = {
    s"attestations=${attestment.hex}"
  }
}

object CreateAttestations {

  def apply(
      announcement: OracleAnnouncementTLV,
      attestment: OracleAttestmentV0TLV): CreateAttestations = {
    CreateAttestations(announcement.sha256, attestment)
  }
}
