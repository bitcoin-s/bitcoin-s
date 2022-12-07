package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.crypto.{ExtKey, ExtPublicKey}
import org.bitcoins.core.hd.BIP32Path

case class DescriptorIterator(descriptor: String) {
  private var index: Int = 0

  def current: String = {
    descriptor.drop(index)
  }

  def skip(numChars: Int): Unit = {
    index += numChars
    ()
  }

  def takeDescriptorType(): DescriptorType = {
    val t = DescriptorType.fromString(current)
    skip(t.toString.length)
    skip(1) //skip the '(' in 'wpkh('
    t
  }

  def takeXPubHDPath(): XPubHDPath = {
    val xpubStr = current.take(ExtPublicKey.base58Len)
    val extPubKey = ExtPublicKey.fromString(xpubStr)
    skip(ExtKey.toString(extPubKey).length)
    val pathStr = current.takeWhile(_ != ')') //parse hd path until we reach ')'
    val hdPath = BIP32Path.fromString("m" + pathStr)
    skip(hdPath.toString.length)
    XPubHDPath(extPubKey, hdPath)
  }
}
