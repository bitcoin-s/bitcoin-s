package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.hd.{BIP32Path}
import org.bitcoins.crypto.StringFactory

case class KeyOriginExpression(fingerprint: String, path: BIP32Path) {

  override def toString: String = {
    s"[${fingerprint + path.toString.drop(1)}]"
  }
}

object KeyOriginExpression extends StringFactory[KeyOriginExpression] {

  override def fromString(string: String): KeyOriginExpression = {
    require(string.head == '[' && string.last == ']',
            s"KeyOriginDescriptor must start and end with [], got=$string")
    val payload = string.drop(1).dropRight(1)
    val fingerprint = payload.take(8)
    val pathElements = payload.drop(8)
    val path = BIP32Path.fromString("m" + pathElements)
    KeyOriginExpression(fingerprint, path)
  }
}
