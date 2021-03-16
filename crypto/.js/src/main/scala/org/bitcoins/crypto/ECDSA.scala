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
    xof: js.Dynamic = js.constructorOf[SHA256],
    pre: String = null)
    extends js.Object {

  def privateKeyGenerate(): Buffer = js.native

  def privateKeyVerify(key: Buffer): Boolean = js.native

  def privateKeyTweakMul(key: Buffer, tweak: Buffer): Buffer =
    js.native

  def publicKeyCreate(key: Buffer, compressed: Boolean): Buffer = js.native

  def publicKeyVerify(key: Buffer): Boolean = js.native

  def publicKeyConvert(key: Buffer, compressed: Boolean): Buffer = js.native

  def publicKeyTweakMul(key: Buffer, tweak: Buffer, compress: Boolean): Buffer =
    js.native

  def publicKeyTweakAdd(key: Buffer, tweak: Buffer, compress: Boolean): Buffer =
    js.native

  def publicKeyCombine(keys: js.Array[Buffer], compress: Boolean): Buffer =
    js.native

  def sign(msg: Buffer, key: Buffer): Buffer = js.native

  def verify(msg: Buffer, sig: Buffer, key: Buffer): Boolean = js.native

  def recover(
      msg: Buffer,
      sig: Buffer,
      param: Byte,
      compress: Boolean): Buffer = js.native

  val curve: js.Dynamic = js.native
}
