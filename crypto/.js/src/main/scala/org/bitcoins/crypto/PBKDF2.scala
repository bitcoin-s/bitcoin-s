package org.bitcoins.crypto

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("bcrypto/lib/pbkdf2.js", JSImport.Default)
class PBKDF2 extends js.Object {

  def derive(
      hash: Hasher,
      pass: Buffer,
      salt: Buffer,
      iter: Int,
      len: Int): Buffer = js.native
}
