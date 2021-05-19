package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.db.DbRowAutoInc
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto._

case class OracleAnnouncementDataDb(
    id: Option[Long],
    announcementSignature: SchnorrDigitalSignature,
    publicKey: SchnorrPublicKey,
    signingPublicKey: SchnorrPublicKey,
    eventId: String,
    eventDescriptor: EventDescriptorTLV,
    eventMaturity: UInt32
) extends DbRowAutoInc[OracleAnnouncementDataDb] {

  override def copyWithId(newId: Long): OracleAnnouncementDataDb =
    copy(id = Some(newId))
}

object OracleAnnouncementDbHelper {

  def fromAnnouncement(tlv: OracleAnnouncementTLV): OracleAnnouncementDataDb = {
    OracleAnnouncementDataDb(None,
                             tlv.announcementSignature,
                             tlv.publicKey,
                             tlv.publicKey,
                             tlv.eventTLV.eventId,
                             tlv.eventTLV.eventDescriptor,
                             tlv.eventTLV.eventMaturityEpoch)

  }

  def fromAnnouncements(
      announcementTLVs: Vector[OracleAnnouncementTLV]): Vector[
    OracleAnnouncementDataDb] = {
    announcementTLVs.map(fromAnnouncement)
  }
}
