package org.bitcoins.crypto

import scala.scalajs.js

/** All hash classes in bcrypto have this structure */
trait Hasher extends js.Object {
  def init(): Unit

  def update(bytes: Buffer): Unit

  def `final`(): Buffer
}
