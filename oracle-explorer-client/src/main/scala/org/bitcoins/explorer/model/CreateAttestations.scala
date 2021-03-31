package org.bitcoins.explorer.model

import org.bitcoins.core.protocol.tlv.OracleAttestmentV0TLV
import org.bitcoins.crypto.Sha256Digest

case class CreateAttestations(
    announcementHash: Sha256Digest,
    attestment: OracleAttestmentV0TLV) {

  override def toString: String = {
    s"attestations=${attestment.hex}"
  }
}
