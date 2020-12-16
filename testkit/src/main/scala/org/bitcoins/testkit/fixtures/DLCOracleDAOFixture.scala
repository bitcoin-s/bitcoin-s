package org.bitcoins.testkit.fixtures

import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.dlc.oracle.storage._
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.scalatest._

import scala.concurrent.Future

case class DLCOracleDAOs(
    rValueDAO: RValueDAO,
    eventDAO: EventDAO,
    outcomeDAO: EventOutcomeDAO)

trait DLCOracleDAOFixture extends BitcoinSFixture with EmbeddedPg {

  implicit protected val config: DLCOracleAppConfig =
    BitcoinSTestAppConfig.getDLCOracleWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = DLCOracleDAOs

  private lazy val daos = {
    val rValueDAO = RValueDAO()
    val eventDAO = EventDAO()
    val outcomeDAO = EventOutcomeDAO()
    DLCOracleDAOs(rValueDAO, eventDAO, outcomeDAO)
  }

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(
      build = () => {
        Future(config.migrate()).map(_ => daos)
      },
      destroy = () => dropAll()
    )(test)
  }

  private def dropAll(): Future[Unit] = {
    val res = for {
      _ <- config.dropTable("flyway_schema_history")
      _ <- config.dropAll()
    } yield ()
    res.failed.foreach { ex =>
      ex.printStackTrace()
    }
    res
  }
}
