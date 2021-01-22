package org.bitcoins.db

import java.nio.file.{Path, Paths}

import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.ExecutionContext

trait AppConfigFactory[C <: AppConfig] {

  def moduleName: String

  def fromConfig(config: Config)(implicit ec: ExecutionContext): C = {
    val configDataDir: Path = Paths.get(config.getString("bitcoin-s.datadir"))
    fromDatadir(configDataDir, Vector(config))
  }

  def fromClassPathConfig()(implicit ec: ExecutionContext): C = {
    fromConfig(ConfigFactory.load())
  }

  def fromDefaultDatadir(confs: Vector[Config] = Vector.empty)(implicit
      ec: ExecutionContext): C = {
    fromDatadir(AppConfig.DEFAULT_BITCOIN_S_DATADIR, confs)
  }

  def fromDatadir(datadir: Path, confs: Vector[Config] = Vector.empty)(implicit
      ec: ExecutionContext): C
}
