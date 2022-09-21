package org.bitcoins.core.dlc.oracle

import org.bitcoins.core.protocol.tlv.OracleMetadata

/** Oracle metadata with the announcement id in the database */
case class OracleMetadataWithId(announcementId: Long, metadata: OracleMetadata)
