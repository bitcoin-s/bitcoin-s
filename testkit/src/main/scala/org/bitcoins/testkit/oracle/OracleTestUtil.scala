package org.bitcoins.testkit.oracle

import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.testkit.util.FileUtil

import scala.concurrent.{ExecutionContext, Future}

object OracleTestUtil {

  def destroyDLCOracleAppConfig(
      config: DLCOracleAppConfig
  )(implicit ec: ExecutionContext): Future[Unit] = {
    // Stop the connection pool before cleaning so that SQLite file locks are
    // released prior to Flyway attempting DDL operations (DROP TABLE).
    for {
      _ <- config.stop()
      _ = config.clean()
      _ = FileUtil.deleteTmpDir(config.baseDatadir)
    } yield ()
  }
}
