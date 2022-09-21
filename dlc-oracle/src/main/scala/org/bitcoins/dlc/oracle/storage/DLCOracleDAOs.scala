package org.bitcoins.dlc.oracle.storage

import org.bitcoins.dlc.commons.oracle.{
  OracleMetadataDAO,
  OracleSchnorrNonceDAO
}

case class DLCOracleDAOs(
    rValueDAO: RValueDAO,
    eventDAO: EventDAO,
    outcomeDAO: EventOutcomeDAO,
    oracleMetadataDAO: OracleMetadataDAO,
    oracleSchnorrNonceDAO: OracleSchnorrNonceDAO)
