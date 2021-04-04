package org.bitcoins.core.api.commons

import java.io.File
import java.nio.file.Path

trait ConfigFactory[T] {
  def empty: T

  def DEFAULT_DATADIR: File

  def DEFAULT_CONF_FILE: File

  def apply(config: String, datadir: File): T

  def apply(config: Path): T =
    apply(config.toFile, config.getParent.toFile)

  def apply(config: File, datadir: File = DEFAULT_DATADIR): T

  def fromConfigFile(file: File = DEFAULT_CONF_FILE): T

  def fromDataDir(dir: File = DEFAULT_DATADIR): T

  def fromDefaultDatadir: T

  def writeConfigToFile(config: T, datadir: File): Path
}
