package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.crypto.{ECPrivateKeyUtil, ExtKey, ExtPublicKey}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.crypto.{ECKeyBytes, ECPublicKeyBytes}

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

  def takeChecksum(): Option[String] = {
    if (current.isEmpty || !(current.take(1) == '#')) {
      //means we do not have a checksum
      None
    } else {
      Some(current.take(8))
    }
  }

  def takeKeyOriginOpt(): Option[KeyOriginExpression] = {
    val (originStr, _) = current.span(_ == ']')
    if (originStr.startsWith("[") && originStr.nonEmpty) {
      val origin = KeyOriginExpression.fromString(originStr)
      skip(origin.toString.length)
      Some(origin)
    } else None
  }

  def takeECKey(): ECKeyBytes = {
    val isPrivKey =
      ECPrivateKeyUtil.privateKeyPrefixes.exists(_ == current.charAt(0))
    val result: ECKeyBytes = if (isPrivKey) {
      ECPrivateKeyUtil.fromWIFToPrivateKey(current)
    } else {
      ECPublicKeyBytes.fromHex(current)
    }
    skip(result.byteSize.toInt)
    result
  }

  def takeExtKey(): ExtKey = {
    ???
  }
}
