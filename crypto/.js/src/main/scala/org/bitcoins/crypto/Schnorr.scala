package org.bitcoins.crypto

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("bcrypto/lib/schnorr.js", JSImport.Default)
class Schnorr extends js.Object {
  def sign(msg: Buffer, key: Buffer, aux: Buffer): Buffer = js.native

  def verify(msg: Buffer, sig: Buffer, key: Buffer): Boolean =
    js.native
}
