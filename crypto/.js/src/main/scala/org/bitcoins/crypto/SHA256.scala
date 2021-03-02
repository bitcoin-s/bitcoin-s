package org.bitcoins.crypto

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Scala wrapper for
  * https://github.com/bcoin-org/bcrypto/blob/4db0feecde86bce71b0c33d31f7178fb14e7381f/lib/js/sha256.js#L54
  */
@js.native
@JSImport("bcrypto/lib/sha256.js", JSImport.Default)
class SHA256() extends Hasher {
  val id: String = js.native

  override def init(): Unit = js.native

  override def update(bytes: Buffer): Unit = js.native

  override def `final`(): Buffer = js.native
}

@js.native
@JSImport("bcrypto/lib/sha256.js", JSImport.Namespace)
object SHA256 extends js.Object {
  val native: String = js.native
  val id: String = js.native
  val size: Int = js.native
  val bits: Int = js.native
  val blockSize: Int = js.native
}
