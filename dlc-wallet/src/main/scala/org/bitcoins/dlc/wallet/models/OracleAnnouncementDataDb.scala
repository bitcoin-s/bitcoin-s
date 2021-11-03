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
    eventDescriptor: BaseEventDescriptor,
    eventMaturity: UInt32
) extends DbRowAutoInc[OracleAnnouncementDataDb] {

  override def copyWithId(newId: Long): OracleAnnouncementDataDb =
    copy(id = Some(newId))
}

object OracleAnnouncementDbHelper {

  def fromAnnouncement(
      tlv: BaseOracleAnnouncement): OracleAnnouncementDataDb = {
    OracleAnnouncementDataDb(
      None,
      tlv.announcementSignature,
      tlv.announcementPublicKey,
      tlv.announcementPublicKey,
      tlv.eventTLV.eventId,
      tlv.eventTLV.eventDescriptor,
      tlv.eventTLV.eventMaturityEpoch
    )

  }

  def fromAnnouncements(
      announcementTLVs: Vector[BaseOracleAnnouncement]): Vector[
    OracleAnnouncementDataDb] = {
    announcementTLVs.map(fromAnnouncement)
  }
}
