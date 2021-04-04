package org.bitcoins.core.api.commons

import java.io.File
import java.nio.file.Path

trait InstanceFactory[+T] {
  def DEFAULT_DATADIR: Path
  def DEFAULT_CONF_FILE: Path

  def fromConfigFile(file: File = DEFAULT_CONF_FILE.toFile): T

  def fromDataDir(dir: File = DEFAULT_DATADIR.toFile): T
}
