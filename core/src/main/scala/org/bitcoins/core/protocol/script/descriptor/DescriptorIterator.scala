package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.crypto.{ECPrivateKeyUtil, ExtKey, ExtPublicKey}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  RawScriptPubKey
}
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
      val (stripped, _) = if (current.exists(_ == '*')) {
        current.span(_ != '*') //remove indicator if all children are hardened
      } else {
        current.span(_ != ')') //else if no hardened indcator, drop last ')'
      }
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

  def takeSingleKeyExpression(): SingleKeyExpression = {
    val singleKeyExpression = SingleKeyExpression.fromString(current)
    skip(singleKeyExpression.toString.length)
    singleKeyExpression
  }

  def takeMultisigKeyExpression(): MultisigKeyExpression = {
    val multisigKeyExpression = MultisigKeyExpression.fromString(current)
    skip(multisigKeyExpression.toString.length)
    multisigKeyExpression
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

  def takeRawSPKScriptExpression(): RawSPKScriptExpression = {
    takeScriptExpression() match {
      case raw: RawSPKScriptExpression => raw
      case x =>
        sys.error(
          s"Unexpected expression=$x when expecting RawSPKScriptExpression")
    }
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

  def takeMultisigSPK(): MultiSignatureScriptPubKey = {
    takeRawScriptPubKey() match {
      case m: MultiSignatureScriptPubKey => m
      case x =>
        sys.error(s"Non multisig SPK found when expected multisigSPK, got=$x")
    }
  }

  def takeTreeExpression(): TreeExpression = {
    val (_, scriptPath) = current.span(_ != ',')
    val singleKeyExpr = takeSingleKeyExpression()
    val keypath = KeyPathOnlyTreeExpression(singleKeyExpr)
    println(s"scriptPath=$scriptPath")
    if (scriptPath.isEmpty) {
      keypath
    } else {
      val treeExpression = takeTapscriptTreeExpression()
      ScriptPathTreeExpression(keypath, treeExpression)
    }

  }

  def takeTapscriptTreeExpression(): TapscriptTreeExpression = {
    val expression = if (current.charAt(1) == '{' && current.last == '}') {
      skip(2) //,{
      val split = current
        .dropRight(1) //}
        .split(',')
      val expressions = split.map(ScriptExpression.fromString).toVector
      println(s"takeTapscriptTreeExpression().0 $expressions")
      TapscriptTreeExpression(expressions)
    } else if (current.charAt(0) == ',') {
      skip(1) //,
      println(s"current=$current")
      val expression = takeScriptExpression()
      println(s"takeTapscriptTreeExpression().1 $expression")
      TapscriptTreeExpression(Vector(expression))
    } else {
      sys.error(s"Cannot parse TapscriptTreeExpression() from current=$current")
    }
    skip(expression.toString.length)
    expression
  }
}
