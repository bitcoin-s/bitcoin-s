package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.hd.HDPath
import org.bitcoins.crypto.StringFactory

case class KeyOriginExpression(fingerprint: String, hdPath: HDPath) {

  override def toString: String = {
    s"[${fingerprint + hdPath.toString}]"
  }
}

object KeyOriginExpression extends StringFactory[KeyOriginExpression] {

  override def fromString(string: String): KeyOriginExpression = {
    require(string.head == '[' && string.last == ']',
            s"KeyOriginDescriptor must start and end with [], got=$string")
    val payload = string.drop(1).dropRight(1)
    val fingerprint = payload.take(8)
    val pathElements = payload.drop(8)
    val path = HDPath.fromString(pathElements)
    KeyOriginExpression(fingerprint, path)
  }
}
