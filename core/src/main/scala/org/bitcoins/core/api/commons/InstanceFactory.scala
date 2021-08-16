package org.bitcoins.core.api.commons

import java.io.File
import java.nio.file.Path

trait InstanceFactory[+T] {
  def fromConfigFile(file: File): T

  def fromDataDir(dir: File): T
}

trait InstanceFactoryLocal[+T] extends InstanceFactory[T] {
  def DEFAULT_DATADIR: Path
  def DEFAULT_CONF_FILE: Path
}
