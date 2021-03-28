package org.bitcoins.crypto.facade

import org.bitcoins.crypto.Hasher

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** The scala js wrapper for
  * https://github.com/bcoin-org/bcrypto/blob/master/lib/js/hash160.js
  */
@js.native
@JSImport("bcrypto/lib/hash160.js", JSImport.Default)
class Hash160 extends Hasher {
  override def init(): Unit = js.native

  override def update(bytes: Buffer): Unit = js.native

  override def `final`(): Buffer = js.native
}
