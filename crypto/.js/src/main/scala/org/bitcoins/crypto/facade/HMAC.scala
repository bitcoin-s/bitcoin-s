package org.bitcoins.crypto.facade

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("bcrypto/lib/internal/hmac.js", JSImport.Default)
class HMAC extends js.Object {
  def init(key: Buffer): Unit = js.native

  def update(bytes: Buffer): Unit = js.native

  def `final`(): Buffer = js.native
}
