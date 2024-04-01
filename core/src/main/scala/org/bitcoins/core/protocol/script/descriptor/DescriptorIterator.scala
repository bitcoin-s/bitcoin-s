package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.crypto.{ECPrivateKeyUtil, ExtKey, ExtPublicKey}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.crypto.{ECKeyBytes, ECPublicKeyBytes}

case class DescriptorIterator(descriptor: String) {
  private var index: Int = 0

  private val hardenedChars: Vector[Char] = {
    Vector('\'', 'h', 'H')
  }

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

  def takeBIP32PathOpt(): Option[BIP32Path] = {
    if (current.nonEmpty && current.charAt(0) == '/') {
      val (stripped, _) =
        current.span(_ != '*') //remove indicator if all children are hardened
      val hdPath = BIP32Path.fromString("m" + stripped)
      skip(hdPath.toString.length)
      Some(hdPath)
    } else {
      None
    }
  }

  def takeChildrenHardenedOpt(): Option[Boolean] = {
    if (current.nonEmpty && current.charAt(0) == '*') {
      skip(1)
      if (current.nonEmpty && hardenedChars.exists(_ == current.charAt(0))) {
        skip(1)
        Some(true)
      } else {
        Some(false)
      }
    } else {
      None
    }
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
    val (originStr, _) = current.span(_ != ']')
    if (originStr.startsWith("[") && originStr.nonEmpty) {
      val origin = KeyOriginExpression
        .fromString(originStr + "]") //span drops the last ']', so re-add
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
    val str = current.take(ExtPublicKey.base58Len)
    val extKey = ExtKey.fromString(str)
    skip(ExtKey.toString(extKey).length)
    extKey
  }
}
