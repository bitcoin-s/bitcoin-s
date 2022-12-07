package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.hd.HDPath
import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, ScriptPubKey}
import org.bitcoins.crypto.StringFactory

/** @see [[https://github.com/bitcoin/bitcoin/blob/master/doc/descriptors.md]]
  */
sealed abstract class OutputDescriptor {
  def scriptPubKey: ScriptPubKey
}

case class P2WPKHDescriptor(xpub: ExtPublicKey, path: HDPath)
    extends OutputDescriptor {

  override val scriptPubKey: P2WPKHWitnessSPKV0 = {
    val pubKey = xpub.deriveChildPubKey(path).get.key
    P2WPKHWitnessSPKV0(pubKey)
  }
}

object P2WPKHDescriptor extends StringFactory[P2WPKHDescriptor] {

  override def fromString(string: String): P2WPKHDescriptor = {
    ???
  }
}
