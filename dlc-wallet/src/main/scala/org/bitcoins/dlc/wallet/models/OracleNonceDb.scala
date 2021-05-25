package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.protocol.tlv.OracleAnnouncementTLV
import org.bitcoins.crypto._

case class OracleNonceDb(
    announcementId: Long,
    index: Long,
    announcementSignature: SchnorrDigitalSignature,
    nonce: SchnorrNonce,
    signatureOpt: Option[SchnorrDigitalSignature],
    outcomeOpt: Option[String]
)

object OracleNonceDbHelper {

  def fromAnnouncement(
      id: Long,
      tlv: OracleAnnouncementTLV): Vector[OracleNonceDb] = {
    tlv.eventTLV.nonces.zipWithIndex.map { case (nonce, index) =>
      OracleNonceDb(id, index, SchnorrDigitalSignature.dummy, nonce, None, None)
    }
  }

  def fromAnnouncements(
      announcementsWithId: Vector[(OracleAnnouncementTLV, Long)]): Vector[
    OracleNonceDb] = {
    announcementsWithId.flatMap { case (announcement, id) =>
      fromAnnouncement(id, announcement)
    }
  }
}
