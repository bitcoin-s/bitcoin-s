package org.bitcoins.crypto.facade

import org.bitcoins.crypto.Hasher

import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
@JSImport("bcrypto/lib/sha3-256.js", JSImport.Default)
class SHA3_256 extends Hasher {

  override def init(): Unit = js.native

  override def update(bytes: Buffer): Unit = js.native

  override def `final`(): Buffer = js.native
}
