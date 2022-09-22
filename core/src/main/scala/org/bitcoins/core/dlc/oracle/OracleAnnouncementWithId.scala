package org.bitcoins.core.dlc.oracle

import org.bitcoins.core.protocol.tlv.BaseOracleAnnouncement

case class OracleAnnouncementWithId(
    id: Long,
    announcement: BaseOracleAnnouncement)
