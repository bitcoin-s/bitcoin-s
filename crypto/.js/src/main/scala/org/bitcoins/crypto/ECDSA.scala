package org.bitcoins.crypto

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Scala wrapper for
  * https://github.com/bcoin-org/bcrypto/blob/master/lib/js/ecdsa.js
  */
@js.native
@JSImport("bcrypto/lib/js/ecdsa.js", JSImport.Default)
class ECDSA(
    name: String = "SECP256K1",
    hash: SHA256 = new SHA256,
    xof: SHA256 = new SHA256,
    pre: String = null)
    extends js.Object {

  def publicKeyCreate(key: Buffer, compressed: Boolean): Buffer = js.native
}
