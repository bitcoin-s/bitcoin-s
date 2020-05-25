package org.bitcoins.db

import java.nio.file.Path

import com.typesafe.config.Config

import scala.concurrent.ExecutionContext

trait AppConfigFactory[C <: AppConfig] {

  def fromDefaultDatadir(useLogbackConf: Boolean, confs: Vector[Config] = Vector.empty)(
    implicit ec: ExecutionContext): C = {
    fromDatadir(AppConfig.DEFAULT_BITCOIN_S_DATADIR,useLogbackConf,confs)
  }

  def fromDatadir(datadir: Path, useLogbackConf: Boolean, confs: Vector[Config] = Vector.empty)(implicit ec: ExecutionContext): C
}
