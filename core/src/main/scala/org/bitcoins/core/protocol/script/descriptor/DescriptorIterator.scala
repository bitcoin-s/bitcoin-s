package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.crypto.{ECPrivateKeyUtil, ExtKey, ExtPublicKey}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.protocol.script.RawScriptPubKey
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  ECKeyBytes,
  ECPublicKeyBytes,
  Sha256Hash160Digest
}

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

  def takeChecksumOpt(): Option[String] = {
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
    val (keyBytes, _) = current.span(_ != ')')
    val result: ECKeyBytes = if (isPrivKey) {
      val k = ECPrivateKeyUtil.fromWIFToPrivateKey(keyBytes)
      k
    } else {
      ECPublicKeyBytes.fromHex(keyBytes)
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

  def takeKeyExpression(): KeyExpression = {
    val keyExpression = KeyExpression.fromString(current)
    skip(keyExpression.toString.length)
    keyExpression
  }

  def takeDoubleSha256DigestBE(): DoubleSha256DigestBE = {
    val hash = DoubleSha256DigestBE.fromHex(current.take(32))
    skip(hash.byteSize.toInt)
    hash
  }

  def takeSha256Hash160(): Sha256Hash160Digest = {
    val hash = Sha256Hash160Digest.fromHex(current.take(32))
    skip(hash.byteSize.toInt)
    hash
  }

  def takeScriptExpression(): ScriptExpression = {
    val expression = ScriptExpression.fromString(current)
    skip(expression.toString.length)
    expression
  }

  def takeRawScriptPubKey(): RawScriptPubKey = {
    val spk = RawScriptPubKey.fromAsmHex(current)
    skip(spk.byteSize.toInt)
    spk
  }
}
