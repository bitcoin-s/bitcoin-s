package org.bitcoins.dlc.oracle.storage

import org.bitcoins.dlc.commons.oracle.{
  EventOutcomeDAO,
  OracleAnnouncementDataDAO,
  OracleMetadataDAO,
  OracleSchnorrNonceDAO
}

case class DLCOracleDAOs(
    rValueDAO: RValueDAO,
    eventDAO: EventDAO,
    outcomeDAO: EventOutcomeDAO,
    oracleAnnouncementDAO: OracleAnnouncementDataDAO,
    oracleMetadataDAO: OracleMetadataDAO,
    oracleSchnorrNonceDAO: OracleSchnorrNonceDAO)
