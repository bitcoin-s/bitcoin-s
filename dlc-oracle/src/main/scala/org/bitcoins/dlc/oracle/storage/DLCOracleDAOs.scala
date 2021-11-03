package org.bitcoins.dlc.oracle.storage

case class DLCOracleDAOs(
    rValueDAO: RValueDAO,
    eventDAO: EventDAO,
    outcomeDAO: EventOutcomeDAO,
    oracleMetadataDAO: OracleMetadataDAO,
    oracleSchnorrNonceDAO: OracleSchnorrNonceDAO)
