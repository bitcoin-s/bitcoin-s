package org.bitcoins.core.api.commons

import java.io.File
import java.nio.file.Path

/** A factory to create things like bitcoind instances or eclair instances
  * @tparam the type of the instance (i.e. BitcoindInstance)
  * @tparam the type of hte implicit parameter, this can be an execution context or ActorSystem
  */
trait InstanceFactory[+T, I] {
  def fromConfigFile(file: File)(implicit i: I): T

  def fromDataDir(dir: File)(implicit i: I): T
}

trait InstanceFactoryLocal[+T, I] extends InstanceFactory[T, I] {
  def DEFAULT_DATADIR: Path
  def DEFAULT_CONF_FILE: Path
}
