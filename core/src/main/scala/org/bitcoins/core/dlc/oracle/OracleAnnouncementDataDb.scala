package org.bitcoins.core.dlc.oracle

import org.bitcoins.core.api.db.DbRowAutoInc
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto._

case class OracleAnnouncementDataDb(
    id: Option[Long],
    announcementSignature: SchnorrDigitalSignature,
    announcementPublicKey: SchnorrPublicKey,
    attestationPublicKey: SchnorrPublicKey,
    eventId: String,
    eventDescriptor: BaseEventDescriptor,
    eventMaturity: UInt32
) extends DbRowAutoInc[OracleAnnouncementDataDb] {

  override def copyWithId(newId: Long): OracleAnnouncementDataDb =
    copy(id = Some(newId))
}

object OracleAnnouncementDbHelper {

  def fromAnnouncement(
      tlv: BaseOracleAnnouncement): OracleAnnouncementDataDb = {
    tlv match {
      case _: OracleAnnouncementV0TLV =>
        OracleAnnouncementDataDb(
          None,
          tlv.announcementSignature,
          tlv.announcementPublicKey,
          tlv.announcementPublicKey,
          tlv.eventTLV.eventId,
          tlv.eventTLV.eventDescriptor,
          tlv.eventTLV.eventMaturityEpoch
        )
      case v1: OracleAnnouncementV1TLV =>
        OracleAnnouncementDataDb(
          id = None,
          announcementSignature = tlv.announcementSignature,
          announcementPublicKey = v1.announcementPublicKey,
          attestationPublicKey = v1.attestationPublicKey,
          eventId = tlv.eventTLV.eventId,
          eventDescriptor = tlv.eventTLV.eventDescriptor,
          eventMaturity = tlv.eventTLV.eventMaturityEpoch
        )
    }

  }

  def fromAnnouncements(
      announcementTLVs: Vector[BaseOracleAnnouncement]): Vector[
    OracleAnnouncementDataDb] = {
    announcementTLVs.map(fromAnnouncement)
  }
}
