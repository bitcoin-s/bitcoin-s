package org.bitcoins.crypto

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js._
import org.bitcoins.crypto.Buffer

@js.native
@JSImport("bcrypto/lib/random.js", JSImport.Namespace)
object Random extends js.Object {
  def randomBytes(n: Int): Buffer = js.native
}

@js.native
@JSImport("bcrypto/lib/random-browser.js", JSImport.Namespace)
object RandomBrowser extends js.Object {
  def randomBytes(n: Int): Buffer = js.native
}
