package org.bitcoins.mempool.config

import com.typesafe.config.Config
import org.bitcoins.commons.config.AppConfig

import java.nio.file.Path
import scala.concurrent.Future

case class MempoolAppConfig(baseDatadir: Path, configOverrides: Vector[Config])
    extends AppConfig {

  /** Sub members of AppConfig should override this type with the type of
    * themselves, ensuring `withOverrides` return the correct type
    */
  override protected[bitcoins] type ConfigType = MempoolAppConfig

  /** Constructor to make a new instance of this config type */
  override protected[bitcoins] def newConfigOfType(
      configOverrides: Vector[Config]): MempoolAppConfig = {
    MempoolAppConfig(baseDatadir, configOverrides)
  }

  /** Name of the module. `chain`, `wallet`, `node` etc.
    */
  override private[bitcoins] def moduleName: String = "mempool"

  override def stop(): Future[Unit] = Future.unit
}
