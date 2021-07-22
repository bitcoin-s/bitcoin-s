package org.bitcoins.testkit.oracle

import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.testkit.util.FileUtil

import scala.concurrent.{ExecutionContext, Future}

object OracleTestUtil {

  def destroyDLCOracleAppConfig(config: DLCOracleAppConfig)(implicit
      ec: ExecutionContext): Future[Unit] = {
    val _ = config.clean()
    for {
      _ <- config.stop()
      _ = FileUtil.deleteTmpDir(config.baseDatadir)
    } yield ()
  }
}
