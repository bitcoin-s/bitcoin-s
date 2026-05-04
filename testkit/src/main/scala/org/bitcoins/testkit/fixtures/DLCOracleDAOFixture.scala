package org.bitcoins.testkit.fixtures

import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.dlc.oracle.storage._
import org.bitcoins.testkit.{BitcoinSTestAppConfig, PostgresTestDatabase}
import org.scalatest._

import scala.concurrent.Future

case class DLCOracleDAOs(
    rValueDAO: RValueDAO,
    eventDAO: EventDAO,
    outcomeDAO: EventOutcomeDAO
)(implicit val oracleAppConfig: DLCOracleAppConfig)

trait DLCOracleDAOFixture extends BitcoinSFixture with PostgresTestDatabase {

  private def config: DLCOracleAppConfig =
    BitcoinSTestAppConfig.getDLCOracleWithEmbeddedDbTestConfig(postgresOpt)

  override type FixtureParam = DLCOracleDAOs

  private def daos()(implicit
      oracleAppConfig: DLCOracleAppConfig): DLCOracleDAOs = {
    val rValueDAO = RValueDAO()
    val eventDAO = EventDAO()
    val outcomeDAO = EventOutcomeDAO()
    DLCOracleDAOs(rValueDAO, eventDAO, outcomeDAO)
  }

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[DLCOracleDAOs](
      build = () => {
        val c = config
        c.start().map(_ => daos()(using c))
      },
      destroy = { (daos: DLCOracleDAOs) => dropAll(daos.oracleAppConfig) }
    )(test)
  }

  private def dropAll(config: DLCOracleAppConfig): Future[Unit] = {
    // Stop the connection pool before cleaning so that SQLite file locks are
    // released prior to Flyway attempting DDL operations (DROP TABLE).
    for {
      _ <- config.stop()
      _ = config.clean()
    } yield ()
  }
}
