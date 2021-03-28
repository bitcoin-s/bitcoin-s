package org.bitcoins.crypto.facade

import org.bitcoins.crypto.Hasher

import scala.scalajs.js
import scala.scalajs.js.annotation._

//the annotation specifies that Foobaz is a native JS class defined in the module "bar.js", and exported under the name "Foo".
@js.native
@JSImport("bcrypto/lib/ripemd160.js", JSImport.Default)
class RipeMd160 extends Hasher {

  override def init(): Unit = js.native

  override def update(bytes: Buffer): Unit = js.native

  override def `final`(): Buffer = js.native
}
