package org.bitcoins.testkit.fixtures

import org.bitcoins.db.DatabaseDriver.{PostgreSQL, SQLite}
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.dlc.oracle.storage._
import org.bitcoins.testkit.{BitcoinSTestAppConfig, PostgresTestDatabase}
import org.scalatest._

import java.nio.file.Files
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
        c.start().map(_ => daos()(c))
      },
      destroy = { (daos: DLCOracleDAOs) => dropAll(daos.oracleAppConfig) }
    )(test)
  }

  private def dropAll(config: DLCOracleAppConfig): Future[Unit] = {
    for {
      _ <- config.stop()
      _ = config.driver match {
        case SQLite =>
          Files.deleteIfExists(config.dbPath.resolve(config.dbName))
          Files.deleteIfExists(config.dbPath.resolve(config.dbName + "-wal"))
          Files.deleteIfExists(config.dbPath.resolve(config.dbName + "-shm"))
        case PostgreSQL =>
          config.clean()
      }
    } yield ()
  }
}
