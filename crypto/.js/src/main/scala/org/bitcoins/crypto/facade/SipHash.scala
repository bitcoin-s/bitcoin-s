package org.bitcoins.crypto.facade

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("bcrypto/lib/siphash.js", JSImport.Namespace)
object SipHash extends js.Object {

  def siphash(data: Buffer, key: Buffer): js.Array[Int] = js.native

}
