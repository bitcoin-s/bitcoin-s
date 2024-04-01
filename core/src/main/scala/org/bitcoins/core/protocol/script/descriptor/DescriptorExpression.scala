package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{
  ECPrivateKeyUtil,
  ExtKey,
  ExtPrivateKey,
  ExtPublicKey
}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.protocol.script.{
  P2PKHScriptPubKey,
  P2SHScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WSHWitnessSPKV0,
  RawScriptPubKey,
  ScriptPubKey
}
import org.bitcoins.crypto.{
  BaseECKey,
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

  def key: BaseECKey
}

sealed abstract class PrivateKeyExpression extends KeyExpression {
  override def key: ECPrivateKey
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
    bytes: ECPrivateKeyBytes,
    network: NetworkParameters,
    originOpt: Option[KeyOriginExpression])
    extends PrivateKeyExpression {
  override val key: ECPrivateKey = bytes.toPrivateKey

  override def toString(): String = {
    originOpt.map(_.toString).getOrElse("") +
      ECPrivateKeyUtil.toWIF(bytes, network)
  }
}

sealed abstract class PublicKeyExpression extends KeyExpression {
  override def key: ECPublicKey
}

/** A key expression that looks like
  * 0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce600
  * [deadbeef/0h/0h/0h]0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce600
  * @param bytes
  * @param originOpt
  */
case class RawPublicKeyExpression(
    bytes: ECPublicKeyBytes,
    originOpt: Option[KeyOriginExpression])
    extends PublicKeyExpression {
  override val key: ECPublicKey = bytes.toPublicKey

  override def toString(): String = {
    originOpt.map(_.toString).getOrElse("") +
      bytes.hex
  }
}

sealed abstract class ExtKeyExpression extends KeyExpression {
  def extKey: ExtKey

  def pathOpt: Option[BIP32Path]

  def childrenHardenedOpt: Option[Boolean]

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

  override val key: ECPrivateKey = {
    pathOpt match {
      case Some(path) =>
        extKey
          .deriveChildPrivKey(path)
          .key
      case None => extKey.key
    }
  }
}

case class XpubKeyExpression(
    override val extKey: ExtPublicKey,
    originOpt: Option[KeyOriginExpression],
    pathOpt: Option[BIP32Path],
    childrenHardenedOpt: Option[Boolean])
    extends ExtKeyExpression {

  override val key: ECPublicKey = {
    pathOpt match {
      case Some(path) =>
        extKey
          .deriveChildPubKey(path)
          .get
          .key
      case None => extKey.key
    }
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
              RawPrivateKeyExpression(bytes = priv,
                                      network = network,
                                      originOpt = keyOriginOpt)
            case Failure(err) => throw err
          }
        case pub: ECPublicKeyBytes =>
          RawPublicKeyExpression(bytes = pub, originOpt = keyOriginOpt)
      }
    }

  }
}

sealed abstract class ScriptExpression extends DescriptorExpression {
  def scriptPubKey: ScriptPubKey

  def descriptorType: DescriptorType

  override def toString: String = {
    s"${descriptorType.toString}(${scriptPubKey.asmHex})"
  }
}

case class RawScriptExpression(scriptPubKey: RawScriptPubKey)
    extends ScriptExpression {
  override val descriptorType: DescriptorType.Raw.type = DescriptorType.Raw
}

case class P2PKHScriptExpression(keyExpression: KeyExpression)
    extends ScriptExpression {
  override val descriptorType: DescriptorType.PKH.type = DescriptorType.PKH

  override val scriptPubKey: P2PKHScriptPubKey = {
    val pub = keyExpression.key match {
      case priv: ECPrivateKey => priv.publicKey
      case pub: ECPublicKey   => pub
    }
    P2PKHScriptPubKey(pub)
  }
}

case class P2WPKHExpression(keyExpression: KeyExpression)
    extends ScriptExpression {
  override val descriptorType: DescriptorType.WPKH.type = DescriptorType.WPKH

  override val scriptPubKey: P2WPKHWitnessSPKV0 = {
    val pubKey = keyExpression.key match {
      case priv: ECPrivateKey => priv.publicKey
      case pub: ECPublicKey   => pub
    }
    P2WPKHWitnessSPKV0(pubKey)
  }
}

case class P2WSHExpression(scriptExpression: ScriptExpression)
    extends ScriptExpression {
  override val descriptorType: DescriptorType.WSH.type = DescriptorType.WSH

  override val scriptPubKey: P2WSHWitnessSPKV0 = {
    P2WSHWitnessSPKV0(scriptExpression.scriptPubKey)
  }
}

case class P2SHExpression(scriptExpression: ScriptExpression)
    extends ScriptExpression {
  override val descriptorType: DescriptorType.SH.type = DescriptorType.SH

  override val scriptPubKey: P2SHScriptPubKey = P2SHScriptPubKey(
    scriptExpression.scriptPubKey)
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
      case x @ (DescriptorType.TR | DescriptorType.SortedMulti |
          DescriptorType.Multi | DescriptorType.PK) =>
        sys.error(
          s"Descriptor type not supported yet in ScriptExpression.fromString(), got=$x")
    }
    expression
  }
}
