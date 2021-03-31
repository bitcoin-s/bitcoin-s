package org.bitcoins.crypto.facade

import org.bitcoins.crypto.Hasher

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** Scala wrapper for
  * https://github.com/bcoin-org/bcrypto/blob/master/lib/js/pbkdf2.js
  */
@js.native
@JSImport("bcrypto/lib/pbkdf2.js", JSImport.Namespace)
object PBKDF2 extends js.Object {

  def derive(
      hash: Hasher,
      pass: Buffer,
      salt: Buffer,
      iter: Int,
      len: Int): Buffer = js.native
}
