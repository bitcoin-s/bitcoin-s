package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.crypto.{ECPrivateKeyUtil, ExtKey, ExtPublicKey}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  RawScriptPubKey
}
import org.bitcoins.crypto._

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

  def takeKeyExpression(): KeyExpression[ECPublicKey] = {
    val keyExpression = KeyExpression.fromString(current)
    skip(keyExpression.toString.length)
    keyExpression
  }

  def takeSingleKeyExpression(): SingleKeyExpression[PublicKey] = {
    if (current.exists(_ == ',')) {
      //must be xonly, the
      takeSingleXOnlyPubKeyExpression()
        .asInstanceOf[SingleKeyExpression[PublicKey]]
    } else {
      val keyStr = current.takeWhile(_ != ')')
      if (keyStr.length == 64) {
        takeSingleXOnlyPubKeyExpression()
          .asInstanceOf[SingleKeyExpression[PublicKey]]
      } else {
        takeSingleECKeyExpression()
          .asInstanceOf[SingleKeyExpression[PublicKey]]
      }
    }
  }

  def takeSingleECKeyExpression(): SingleECPublicKeyExpression = {
    val singleKeyExpression = SingleECPublicKeyExpression.fromString(current)
    skip(singleKeyExpression.toString.length)
    singleKeyExpression
  }

  def takeSingleXOnlyPubKeyExpression(): SingleXOnlyPubKeyExpression = {
    val keyExpr = {
      if (current.exists(_ == ',')) {
        //we have a script path
        current.span(_ != ',')._1
      } else {
        //no script path, just internal key
        current.takeWhile(_ != ')')
      }
    }

    val single = SingleXOnlyPubKeyExpression.fromString(keyExpr)
    skip(single.toString().length)
    if (current.nonEmpty) {
      require(current.head == ',' || current.head == ')',
              s"Key was not 32 bytes, got=$descriptor current=$current")
    }
    skip(1) // ','
    single
  }

  def takeInternalPublicKeyExpression(): InternalPublicKeyExpression = {
    val key = current.take(64)
    val xonly = XOnlyPubKey.fromHex(key)
    val i = InternalPublicKeyExpression(xonly)
    skip(i.toString().length)
    i
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
    takeScriptExpressionECKey() match {
      case raw: RawSPKScriptExpression => raw
      case x =>
        sys.error(
          s"Unexpected expression=$x when expecting RawSPKScriptExpression")
    }
  }

  def takeScriptExpressionECKey(): ScriptExpression = {
    val expression = ScriptExpressionECKey.fromString(current)
    skip(expression.toString.length)
    expression
  }

  def takeRawScriptExpressionXOnlyKey(): RawSPKScriptExpression = {
    val expression = ScriptExpressionXOnlyKey.fromString(current)
    skip(expression.toString().length)
    expression match {
      case raw: RawSPKScriptExpression => raw
      case x =>
        sys.error(
          s"Unexpected expression=$x when expecting RawSPKScriptExpression")
    }
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
    val singleKeyExpr = takeSingleXOnlyPubKeyExpression()
    val keypath = KeyPathOnlyTreeExpression(singleKeyExpr)
    if (scriptPath.isEmpty) {
      keypath
    } else {
      val treeExpression = takeTapscriptTreeExpression()
      ScriptPathTreeExpression(keypath, treeExpression)
    }

  }

  def takeTapscriptTreeExpression(): TapscriptTreeExpression = {
    val expression = if (current.charAt(0) == '{' && current.last == '}') {
      skip(1) //{
      val tree1 = takeTapscriptTreeExpression()
      skip(1) //,
      val tree2 = takeTapscriptTreeExpression()
      val branch =
        TapscriptBranchExpression(tree1, tree2)
      skip(1) //}
      branch
    } else {
      val expression = takeRawScriptExpressionXOnlyKey()
      TapscriptLeafExpression(expression)
    }
    expression
  }
}
