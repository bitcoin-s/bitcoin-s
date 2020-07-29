package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.protocol.tlv.OracleAnnouncementTLV
import org.bitcoins.crypto.SchnorrPublicKey

case class OracleAnnouncementDb(
    announcement: OracleAnnouncementTLV,
    publicKey: SchnorrPublicKey) {
  require(
    publicKey == announcement.publicKey,
    s"Must have matching public keys, got ${publicKey.hex} != ${announcement.publicKey.hex}")
}

object OracleAnnouncementDbHelper {

  def fromAnnouncement(tlv: OracleAnnouncementTLV): OracleAnnouncementDb = {
    OracleAnnouncementDb(tlv, tlv.publicKey)
  }

  def fromAnnouncements(
      announcementTLVs: Vector[OracleAnnouncementTLV]): Vector[
    OracleAnnouncementDb] = {
    announcementTLVs.map { tlv =>
      OracleAnnouncementDb(tlv, tlv.publicKey)
    }
  }
}
