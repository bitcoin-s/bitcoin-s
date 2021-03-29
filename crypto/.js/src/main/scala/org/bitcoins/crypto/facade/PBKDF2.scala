package org.bitcoins.crypto.facade

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("bcrypto/lib/pbkdf2.js", JSImport.Default)
class PBKDF2 extends js.Object {

  def derive(
      hash: Dynamic,
      pass: Buffer,
      salt: Buffer,
      iter: Int,
      len: Int): Buffer = js.native
}
