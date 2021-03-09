package org.bitcoins.crypto

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("bcrypto/lib/js/elliptic.js", JSImport.Default)
class Point extends js.Object {
  def getX(): js.BigInt = js.native
  def getY(): js.BigInt = js.native
  def isInfinity(): Boolean = js.native
}
