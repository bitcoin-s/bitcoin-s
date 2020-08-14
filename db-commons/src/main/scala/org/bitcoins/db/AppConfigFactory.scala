package org.bitcoins.db

import java.nio.file.Path

import com.typesafe.config.Config

import scala.concurrent.ExecutionContext

trait AppConfigFactory[C <: AppConfig] {

  def fromDefaultDatadir(confs: Vector[Config] = Vector.empty)(implicit
      ec: ExecutionContext): C = {
    fromDatadir(AppConfig.DEFAULT_BITCOIN_S_DATADIR, confs)
  }

  def fromDatadir(datadir: Path, confs: Vector[Config] = Vector.empty)(implicit
      ec: ExecutionContext): C
}
