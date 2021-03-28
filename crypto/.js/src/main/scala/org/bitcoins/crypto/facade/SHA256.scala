package org.bitcoins.crypto.facade

import org.bitcoins.crypto.Hasher

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Scala wrapper for
  * https://github.com/bcoin-org/bcrypto/blob/4db0feecde86bce71b0c33d31f7178fb14e7381f/lib/js/sha256.js#L54
  */
@js.native
@JSImport("bcrypto/lib/sha256.js", JSImport.Default)
class SHA256 extends Hasher {
  var native: Int = js.native
  var id: String = js.native
  var size: Int = js.native
  var bits: Int = js.native
  var blockSize: Int = js.native
  var zero: Buffer = js.native
  var ctx: js.Dynamic = js.native

  override def init(): Unit = js.native

  override def update(bytes: Buffer): Unit = js.native

  override def `final`(): Buffer = js.native
}

@js.native
@JSImport("bcrypto/lib/sha256.js", JSImport.Namespace)
object SHA256 extends js.Object {
  val native: Int = js.native
  val id: String = js.native
  val size: Int = js.native
  val bits: Int = js.native
  val blockSize: Int = js.native
  val zero: Buffer = js.native
  val ctx: js.Dynamic = js.native
}

object SHA256Factory {

  def create(): SHA256 = {
    val hasher = new SHA256

    // initialize static JS variables
    hasher.native = SHA256.native
    hasher.id = SHA256.id
    hasher.size = SHA256.size
    hasher.bits = SHA256.bits
    hasher.blockSize = SHA256.blockSize
    hasher.zero = SHA256.zero
    hasher.ctx = SHA256.ctx

    hasher
  }
}
