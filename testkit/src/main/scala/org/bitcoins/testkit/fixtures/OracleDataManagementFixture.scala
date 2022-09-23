package org.bitcoins.testkit.fixtures

import org.bitcoins.dlc.commons.oracle.{
  EventOutcomeDAO,
  OracleAnnouncementDataDAO,
  OracleMetadataDAO,
  OracleSchnorrNonceDAO
}
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.dlc.oracle.storage.{
  DLCOracleDAOs,
  EventDAO,
  OracleDataManagement,
  RValueDAO
}
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.flywaydb.core.api.output.CleanResult
import org.scalatest.FutureOutcome

import scala.concurrent.Future

trait OracleDataManagementFixture extends BitcoinSFixture with EmbeddedPg {

  implicit protected val config: DLCOracleAppConfig =
    BitcoinSTestAppConfig.getDLCOracleWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = OracleDataManagement

  private lazy val daos: DLCOracleDAOs = {
    val rValueDAO = RValueDAO()
    val eventDAO = EventDAO()
    val outcomeDAO = EventOutcomeDAO()
    val oracleAnnouncementDAO = OracleAnnouncementDataDAO()
    val oracleMetadataDAO = OracleMetadataDAO()
    val oracleSchnorrNonceDAO = OracleSchnorrNonceDAO()
    DLCOracleDAOs(
      rValueDAO = rValueDAO,
      eventDAO = eventDAO,
      outcomeDAO = outcomeDAO,
      oracleAnnouncementDAO = oracleAnnouncementDAO,
      oracleMetadataDAO = oracleMetadataDAO,
      oracleSchnorrNonceDAO = oracleSchnorrNonceDAO
    )
  }

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(
      build = () => {
        config
          .start()
          .map(_ => OracleDataManagement(daos))
      },
      destroy = () => dropAll()
    )(test)
  }

  private def dropAll(): Future[CleanResult] = {
    Future {
      config.clean()
    }
  }
}
