package org.bitcoins.commons.config

import com.typesafe.config.{Config, ConfigFactory}

import java.nio.file.{Path, Paths}
import scala.concurrent.ExecutionContext

/** @tparam I - the implicit argument. This is usually an execution context or an actor system */
trait AppConfigFactoryBase[C <: AppConfig, I] {
  def moduleName: String

  def fromConfig(config: Config)(implicit i: I): C = {
    val configDataDir: Path = Paths.get(config.getString("bitcoin-s.datadir"))
    fromDatadir(configDataDir, Vector(config))
  }

  def fromClassPathConfig()(implicit i: I): C = {
    fromConfig(ConfigFactory.load())
  }

  def fromDefaultDatadir(confs: Vector[Config] = Vector.empty)(implicit
      i: I): C = {
    fromDatadir(AppConfig.DEFAULT_BITCOIN_S_DATADIR, confs)
  }

  def fromDatadir(datadir: Path, confs: Vector[Config] = Vector.empty)(implicit
      i: I): C
}

trait AppConfigFactory[C <: AppConfig]
    extends AppConfigFactoryBase[C, ExecutionContext]
