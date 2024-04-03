package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{
  ECPrivateKeyUtil,
  ExtKey,
  ExtPrivateKey,
  ExtPublicKey
}
import org.bitcoins.core.hd.{BIP32Node, BIP32Path}
import org.bitcoins.core.protocol.script.{
  P2PKHScriptPubKey,
  P2PKScriptPubKey,
  P2SHScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WSHWitnessSPKV0,
  RawScriptPubKey,
  ScriptPubKey
}
import org.bitcoins.core.script.constant.{BytesToPushOntoStack, ScriptConstant}
import org.bitcoins.core.script.crypto.OP_CHECKSIG
import org.bitcoins.crypto.{
  BaseECKey,
  CryptoUtil,
  ECKeyBytes,
  ECPrivateKey,
  ECPrivateKeyBytes,
  ECPublicKey,
  ECPublicKeyBytes,
  StringFactory
}

import scala.util.{Failure, Success}

sealed abstract class DescriptorExpression

/** Denotes a key expression, examples of which are
  * 0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce600
  * [deadbeef/0h/0h/0h]0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce60
  * [deadbeef/0h/1h/2h]xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcE
  */
sealed abstract class KeyExpression extends DescriptorExpression {
  def originOpt: Option[KeyOriginExpression]

  def key: ECKeyBytes
}

sealed abstract class PrivateKeyExpression extends KeyExpression {
  override def key: ECPrivateKeyBytes
}

/** A private key descriptor expression
  * Examples of what this data structure can represent
  * 5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss
  * L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1
  *
  * Its unclear to me at this point if private keys can have key origin
  * @param bytes
  * @param network
  * @param originOpt
  */
case class RawPrivateKeyExpression(
    key: ECPrivateKeyBytes,
    network: NetworkParameters,
    originOpt: Option[KeyOriginExpression])
    extends PrivateKeyExpression {

  override def toString(): String = {
    originOpt.map(_.toString).getOrElse("") +
      ECPrivateKeyUtil.toWIF(key, network)
  }
}

sealed abstract class PublicKeyExpression extends KeyExpression {
  override def key: ECPublicKeyBytes
}

/** A key expression that looks like
  * 0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce600
  * [deadbeef/0h/0h/0h]0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce600
  * @param bytes
  * @param originOpt
  */
case class RawPublicKeyExpression(
    key: ECPublicKeyBytes,
    originOpt: Option[KeyOriginExpression])
    extends PublicKeyExpression {

  override def toString(): String = {
    originOpt.map(_.toString).getOrElse("") +
      key.hex
  }
}

/** Represents key expressions that are BIP32 keys
  * Examples:
  * xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc
  * [deadbeef/0'/1'/2']xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc/3/4/5
  * [deadbeef/0'/1'/2']xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc/3/4/5/\*
  */
sealed abstract class ExtKeyExpression extends KeyExpression {
  def extKey: ExtKey

  def pathOpt: Option[BIP32Path]

  def childrenHardenedOpt: Option[Boolean]

  def deriveChild(idx: Int): BaseECKey

  override def toString(): String = {
    originOpt.map(_.toString).getOrElse("") +
      ExtKey.toString(extKey) +
      pathOpt.map(_.toString.drop(1)).getOrElse("") +
      childrenHardenedOpt
        .map {
          case true  => "/*'"
          case false => "/*"
        }
        .getOrElse("")
  }
}

case class XprvKeyExpression(
    override val extKey: ExtPrivateKey,
    originOpt: Option[KeyOriginExpression],
    pathOpt: Option[BIP32Path],
    childrenHardenedOpt: Option[Boolean])
    extends ExtKeyExpression {

  override val key: ECPrivateKeyBytes = {
    pathOpt match {
      case Some(path) =>
        extKey
          .deriveChildPrivKey(path)
          .key
          .toPrivateKeyBytes()
      case None =>
        extKey.key.toPrivateKeyBytes()
    }
  }

  override def deriveChild(idx: Int): ECPrivateKey = {
    require(
      childrenHardenedOpt.isDefined,
      s"Cannot derive child keys from descriptor that does not allow children, got=${toString}")
    val node = BIP32Node(index = idx, hardened = childrenHardenedOpt.get)
    val fullPath: BIP32Path = pathOpt match {
      case Some(p) => BIP32Path(p.path.appended(node))
      case None    => BIP32Path(node)
    }
    extKey.deriveChildPrivKey(fullPath).key
  }
}

case class XpubKeyExpression(
    override val extKey: ExtPublicKey,
    originOpt: Option[KeyOriginExpression],
    pathOpt: Option[BIP32Path],
    childrenHardenedOpt: Option[Boolean])
    extends ExtKeyExpression {

  override val key: ECPublicKeyBytes = {
    pathOpt match {
      case Some(path) =>
        extKey
          .deriveChildPubKey(path)
          .get
          .key
          .toPublicKeyBytes()
      case None => extKey.key.toPublicKeyBytes()
    }
  }

  override def deriveChild(idx: Int): ECPublicKey = {
    require(
      childrenHardenedOpt.isDefined,
      s"Cannot derive child keys from descriptor that does not allow children, got=${toString}")
    val node = BIP32Node(index = idx, hardened = childrenHardenedOpt.get)
    val fullPath: BIP32Path = pathOpt match {
      case Some(p) => BIP32Path(p.path.appended(node))
      case None    => BIP32Path(node)
    }
    extKey
      .deriveChildPubKey(fullPath)
      .map(_.key)
      .get // should be safe if we had the hardened indicator?
  }
}

object KeyExpression extends StringFactory[KeyExpression] {

  override def fromString(string: String): KeyExpression = {
    val iter = DescriptorIterator(string)
    val keyOriginOpt = iter.takeKeyOriginOpt()
    val isExtKey = ExtKey.prefixes.exists(p => iter.current.startsWith(p))
    if (isExtKey) {
      val extKey = iter.takeExtKey()
      val pathOpt = iter.takeBIP32PathOpt()
      val childrenHardenedOpt = iter.takeChildrenHardenedOpt()
      extKey match {
        case xprv: ExtPrivateKey =>
          XprvKeyExpression(xprv, keyOriginOpt, pathOpt, childrenHardenedOpt)
        case xpub: ExtPublicKey =>
          XpubKeyExpression(xpub, keyOriginOpt, pathOpt, childrenHardenedOpt)
      }
    } else {
      // needed to parse network info in case of WIF private key
      val (cp, _) = iter.current.span(_ != ')')
      val keyBytes = iter.takeECKey()
      keyBytes match {
        case priv: ECPrivateKeyBytes =>
          val networkT = ECPrivateKeyUtil.parseNetworkFromWIF(cp)
          networkT match {
            case Success(network) =>
              RawPrivateKeyExpression(key = priv,
                                      network = network,
                                      originOpt = keyOriginOpt)
            case Failure(err) => throw err
          }
        case pub: ECPublicKeyBytes =>
          RawPublicKeyExpression(key = pub, originOpt = keyOriginOpt)
      }
    }

  }
}

sealed abstract class ScriptExpression extends DescriptorExpression {
  def scriptPubKey: ScriptPubKey

  def descriptorType: DescriptorType
}

/** The source for where a [[ScriptExpression]] derives its information */
sealed trait ExpressionSource { _: ScriptExpression =>
  def source: DescriptorExpression

  override def toString: String = {
    s"${descriptorType.toString}(${source.toString})"
  }
}

/** A script expression derived from a key expression
  * Example:
  */
sealed trait KeyExpressionScriptExpression extends ExpressionSource {
  _: ScriptExpression =>
  override def source: KeyExpression
}

/** A script expression nested inside of another script expression
  * Example:
  * sh(wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)))
  */
sealed trait NestedScriptExpression extends ExpressionSource {
  _: ScriptExpression =>
  override def source: ScriptExpression
}

case class RawScriptExpression(scriptPubKey: RawScriptPubKey)
    extends ScriptExpression {
  override val descriptorType: DescriptorType.Raw.type = DescriptorType.Raw

  override def toString: String = {
    s"${descriptorType.toString}(${scriptPubKey.asmHex})"
  }
}

case class P2PKHScriptExpression(source: KeyExpression)
    extends ScriptExpression
    with KeyExpressionScriptExpression {
  override val descriptorType: DescriptorType.PKH.type = DescriptorType.PKH

  override val scriptPubKey: P2PKHScriptPubKey = {
    val pub = source.key match {
      case priv: ECPrivateKeyBytes => priv.publicKeyBytes.toPublicKey
      case pub: ECPublicKeyBytes   => pub.toPublicKey
    }

    if (pub.isCompressed) {
      P2PKHScriptPubKey(pub)
    } else {
      //since we seem to always serialize a ECPublicKey as 33 bytes
      //we need to build the raw script here, come back and look at this...
      val hash = CryptoUtil.sha256Hash160(pub.decompressedBytes)
      P2PKHScriptPubKey(hash)
    }

  }
}

case class P2PKScriptExpression(source: KeyExpression)
    extends ScriptExpression
    with KeyExpressionScriptExpression {
  override val descriptorType: DescriptorType.PK.type = DescriptorType.PK

  override val scriptPubKey: P2PKScriptPubKey = {
    val pub = source.key match {
      case priv: ECPrivateKeyBytes =>
        priv.publicKeyBytes.toPublicKey
      case pub: ECPublicKeyBytes => pub.toPublicKey
    }
    if (pub.isCompressed) {
      P2PKScriptPubKey(pub)
    } else {
      //since we seem to always serialize a ECPublicKey as 33 bytes
      //we need to build the raw script here, come back and look at this...
      P2PKScriptPubKey.fromAsm(
        Vector(BytesToPushOntoStack(65),
               ScriptConstant(pub.decompressedBytes),
               OP_CHECKSIG))
    }

  }
}

case class P2WPKHExpression(source: KeyExpression)
    extends ScriptExpression
    with KeyExpressionScriptExpression {
  override val descriptorType: DescriptorType.WPKH.type = DescriptorType.WPKH

  override val scriptPubKey: P2WPKHWitnessSPKV0 = {
    val pubKey = source.key match {
      case priv: ECPrivateKeyBytes => priv.publicKeyBytes.toPublicKey
      case pub: ECPublicKeyBytes   => pub.toPublicKey
    }
    P2WPKHWitnessSPKV0(pubKey)
  }
}

case class P2WSHExpression(source: ScriptExpression)
    extends ScriptExpression
    with NestedScriptExpression {
  override val descriptorType: DescriptorType.WSH.type = DescriptorType.WSH

  override val scriptPubKey: P2WSHWitnessSPKV0 = {
    P2WSHWitnessSPKV0(source.scriptPubKey)
  }
}

case class P2SHExpression(source: ScriptExpression)
    extends ScriptExpression
    with NestedScriptExpression {
  override val descriptorType: DescriptorType.SH.type = DescriptorType.SH

  override val scriptPubKey: P2SHScriptPubKey = P2SHScriptPubKey(
    source.scriptPubKey)
}

object ScriptExpression extends StringFactory[ScriptExpression] {

  override def fromString(string: String): ScriptExpression = {
    val iter = DescriptorIterator(string)
    val descriptorType = iter.takeDescriptorType()
    val expression: ScriptExpression = descriptorType match {
      case DescriptorType.PKH  => P2PKHScriptExpression(iter.takeKeyExpression())
      case DescriptorType.WPKH => P2WPKHExpression(iter.takeKeyExpression())
      case DescriptorType.WSH  => P2WSHExpression(iter.takeScriptExpression())
      case DescriptorType.SH   => P2SHExpression(iter.takeScriptExpression())
      case DescriptorType.Raw  => RawScriptExpression(iter.takeRawScriptPubKey())
      case DescriptorType.PK   => P2PKScriptExpression(iter.takeKeyExpression())
      case x @ (DescriptorType.TR | DescriptorType.SortedMulti |
          DescriptorType.Multi | DescriptorType.PK) =>
        sys.error(
          s"Descriptor type not supported yet in ScriptExpression.fromString(), got=$x")
    }
    expression
  }
}
