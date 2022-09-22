package org.bitcoins.core.dlc.oracle

import org.bitcoins.core.protocol.tlv.OracleAnnouncementV1TLV

case class OracleAnnouncementWithId(
    id: Long,
    announcement: OracleAnnouncementV1TLV)
