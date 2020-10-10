package org.bitcoins.db

import java.nio.file.{Path, Paths}

import com.typesafe.config.Config

import scala.concurrent.ExecutionContext

trait AppConfigFactory[C <: AppConfig] {

  def fromConfig(config: Config)(implicit ec: ExecutionContext): C = {
    val configDataDir: Path = Paths.get(config.getString("bitcoin-s.datadir"))
    fromDatadir(configDataDir, Vector(config))
  }

  def fromDefaultDatadir(confs: Vector[Config] = Vector.empty)(implicit
      ec: ExecutionContext): C = {
    fromDatadir(AppConfig.DEFAULT_BITCOIN_S_DATADIR, confs)
  }

  def fromDatadir(datadir: Path, confs: Vector[Config] = Vector.empty)(implicit
      ec: ExecutionContext): C
}
