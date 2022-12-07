package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, ScriptPubKey}
import org.bitcoins.crypto.StringFactory

/** @see [[https://github.com/bitcoin/bitcoin/blob/master/doc/descriptors.md]]
  */
sealed abstract class OutputDescriptor {
  def scriptPubKey: ScriptPubKey
}

case class P2WPKHDescriptor(xPubHDPath: XPubHDPath) extends OutputDescriptor {
  val xpub = xPubHDPath.xpub
  val hdPath = xPubHDPath.bip32Path

  override val scriptPubKey: P2WPKHWitnessSPKV0 = {
    val pubKey = xpub.deriveChildPubKey(hdPath).get.key
    P2WPKHWitnessSPKV0(pubKey)
  }
}

object P2WPKHDescriptor extends StringFactory[P2WPKHDescriptor] {

  override def fromString(string: String): P2WPKHDescriptor = {
    val iter = DescriptorIterator(string)
    val t = iter.takeDescriptorType()
    if (t != DescriptorType.WPKH) {
      sys.error(s"Incorrect type for p2wpkh descriptor, got=$t")
    } else {
      val xPubHDPath = iter.takeXPubHDPath()
      P2WPKHDescriptor(xPubHDPath)
    }
  }
}
