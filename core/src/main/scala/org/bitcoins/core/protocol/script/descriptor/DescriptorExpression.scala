package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{
  ECPrivateKeyUtil,
  ExtKey,
  ExtPrivateKey,
  ExtPublicKey
}
import org.bitcoins.core.hd.{BIP32Node, BIP32Path}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto._

import scala.util.{Failure, Success}

sealed abstract class DescriptorExpression

/** Denotes a key expression, examples of which are
  * 0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce600
  * [deadbeef/0h/0h/0h]0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce60
  * [deadbeef/0h/1h/2h]xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcE
  */
sealed abstract class KeyExpression extends DescriptorExpression { _: PubKeyTypeExpression =>
  def originOpt: Option[KeyOriginExpression]
}
sealed abstract class SingleKeyExpression extends KeyExpression { _: PubKeyTypeExpression =>
  def key: ECKeyBytes

  def pubKey: ECPublicKey = key match {
    case priv: ECPrivateKeyBytes => priv.toPrivateKey.publicKey
    case pub: ECPublicKeyBytes   => pub.toPublicKey
  }
}

sealed trait PubKeyTypeExpression
sealed trait ECPublicKeyExpression extends PubKeyTypeExpression { _: KeyExpression =>
  def pubKey: ECPublicKey
}

sealed trait XOnlyPublicKeyExpression extends PubKeyTypeExpression { _: KeyExpression =>
  def pubKey: XOnlyPubKey
}

sealed abstract class PrivateKeyExpression extends SingleKeyExpression { _: PubKeyTypeExpression =>
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

sealed abstract class PublicKeyExpression extends SingleKeyExpression {
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

case class XOnlyPublicKeyExpression(xOnlyPubKey: XOnlyPubKey)
    extends SingleKeyExpression {
  override val originOpt: Option[KeyOriginExpression] = None

  override def key: ECKeyBytes = xOnlyPubKey.publicKey.toPublicKeyBytes()

  override def toString(): String = {
    originOpt.map(_.toString).getOrElse("") + xOnlyPubKey.hex
  }

}

/** Represents key expressions that are BIP32 keys
  * Examples:
  * xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc
  * [deadbeef/0'/1'/2']xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc/3/4/5
  * [deadbeef/0'/1'/2']xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc/3/4/5/\*
  */
sealed abstract class ExtKeyExpression extends SingleKeyExpression {
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
    val node =
      BIP32Node(index = idx, hardened = childrenHardenedOpt.getOrElse(false))
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

case class MultisigKeyExpression(
    numSigsRequired: Int,
    keyExpressions: Vector[SingleKeyExpression])
    extends KeyExpression {
  override val originOpt = None

  def pubKeys: Vector[ECPublicKey] = {
    keyExpressions.map(_.key).map {
      case priv: ECPrivateKeyBytes => priv.publicKeyBytes.toPublicKey
      case pub: ECPublicKeyBytes   => pub.toPublicKey
    }
  }

  def sortedPubKeys: Vector[ECPublicKey] = {
    pubKeys.sortBy(_.hex)
  }

  override def toString(): String = {
    s"${numSigsRequired},${keyExpressions.mkString(",")}"
  }
}

/** Example: {
  *      pk(xprvA2JDeKCSNNZky6uBCviVfJSKyQ1mDYahRjijr5idH2WwLsEd4Hsb2Tyh8RfQMuPh7f7RtyzTtdrbdqqsunu5Mm3wDvUAKRHSC34sJ7in334/0),
  *     {
  *       {
  *         pk(xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL),
  *         pk(02df12b7035bdac8e3bab862a3a83d06ea6b17b6753d52edecba9be46f5d09e076)},pk(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)
  *       }
  * }
  * @param scriptExpressions
  */
case class TapscriptTreeExpression(leaves: Vector[ScriptExpression])
    extends DescriptorExpression

object SingleKeyExpression extends StringFactory[SingleKeyExpression] {

  override def fromString(string: String): SingleKeyExpression = {
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
    } else if (
      keyOriginOpt.isEmpty && string.takeWhile(_ != ',').length == 64
    ) {
      val xonly = XOnlyPubKey.fromHex(string.take(64))
      XOnlyPublicKeyExpression(xonly)
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

object MultisigKeyExpression extends StringFactory[MultisigKeyExpression] {

  override def fromString(string: String): MultisigKeyExpression = {
    val (requiredSigsStr, keyExpressionsStr) = string.span(_ != ',')

    val split = keyExpressionsStr
      .drop(1) //drop ','
      .split(',')
      .toVector
    val keyExpressions = split.map(SingleKeyExpression.fromString(_))
    MultisigKeyExpression(requiredSigsStr.toInt, keyExpressions)
  }
}

object KeyExpression extends StringFactory[KeyExpression] {

  override def fromString(string: String): KeyExpression = {
    MultisigKeyExpression
      .fromStringOpt(string)
      .getOrElse(SingleKeyExpression.fromString(string))
  }
}

sealed abstract class ScriptExpression extends DescriptorExpression {
  def scriptPubKey: ScriptPubKey
  def descriptorType: DescriptorType
}

sealed abstract class RawSPKScriptExpression extends ScriptExpression {
  override def scriptPubKey: RawScriptPubKey
}

sealed abstract class MultisigScriptExpression
    extends RawSPKScriptExpression
    with KeyExpressionScriptExpression {
  override def scriptPubKey: MultiSignatureScriptPubKey

  override def source: MultisigKeyExpression

  def isSorted: Boolean = descriptorType == DescriptorType.SortedMulti

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
  * tr(a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)
  * tr(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)
  * pkh([bd16bee5/2147483647']xpub69H7F5dQzmVd3vPuLKtcXJziMEQByuDidnX3YdwgtNsecY5HRGtAAQC5mXTt4dsv9RzyjgDjAQs9VGVV6ydYCHnprc9vvaA5YtqWyL6hyds/0)
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

sealed trait TapscriptTree extends ExpressionSource { _: TreeExpression =>
  override def source: TapscriptTreeExpression
}

/** Examples:
  * raw(deadbeef)
  * raw(a9149a4d9901d6af519b2a23d4a2f51650fcba87ce7b87)
  */
case class RawScriptExpression(scriptPubKey: RawScriptPubKey)
    extends RawSPKScriptExpression {
  override val descriptorType: DescriptorType.Raw.type = DescriptorType.Raw

  override def toString: String = {
    s"${descriptorType.toString}(${scriptPubKey.asmHex})"
  }
}

case class P2PKHScriptExpression(source: SingleKeyExpression)
    extends RawSPKScriptExpression
    with KeyExpressionScriptExpression {
  override val descriptorType: DescriptorType.PKH.type = DescriptorType.PKH

  override val scriptPubKey: P2PKHScriptPubKey = {
    val pub = source.key match {
      case priv: ECPrivateKeyBytes => priv.publicKeyBytes.toPublicKey
      case pub: ECPublicKeyBytes   => pub.toPublicKey
    }
    P2PKHScriptPubKey(pub)
  }
}

case class P2PKScriptExpression(source: SingleKeyExpression)
    extends RawSPKScriptExpression
    with KeyExpressionScriptExpression {
  override val descriptorType: DescriptorType.PK.type = DescriptorType.PK

  override val scriptPubKey: P2PKScriptPubKey = {
    val pub = source.key match {
      case priv: ECPrivateKeyBytes =>
        priv.publicKeyBytes.toPublicKey
      case pub: ECPublicKeyBytes => pub.toPublicKey
    }
    P2PKScriptPubKey(pub)
  }
}

case class P2WPKHExpression(source: SingleKeyExpression)
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

case class P2WSHExpression(source: RawSPKScriptExpression)
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

  override val scriptPubKey: P2SHScriptPubKey = {
    source.scriptPubKey match {
      case m: MultiSignatureScriptPubKey =>
        require(m.requiredSigs <= 15,
                s"P2SHExpressions required <= 15 sigs, got=${m.requiredSigs}")
        P2SHScriptPubKey(m)
      case _ => P2SHScriptPubKey(source.scriptPubKey)
    }

  }
}

case class MultisigExpression(source: MultisigKeyExpression)
    extends MultisigScriptExpression
    with KeyExpressionScriptExpression {
  override val descriptorType: DescriptorType.Multi.type = DescriptorType.Multi

  override val scriptPubKey: MultiSignatureScriptPubKey = {
    MultiSignatureScriptPubKey(source.numSigsRequired, source.pubKeys)
  }

  require(
    scriptPubKey.requiredSigs > 0,
    s"Must have positive requiredSigs in MultisigScriptExpression, got=${scriptPubKey.requiredSigs}")

  require(
    scriptPubKey.requiredSigs <= scriptPubKey.maxSigs,
    s"Required sigs greater than max sigs, got requiredSigs=${scriptPubKey.requiredSigs} maxSigs=${scriptPubKey.maxSigs}"
  )
}

case class SortedMultisigExpression(source: MultisigKeyExpression)
    extends MultisigScriptExpression
    with KeyExpressionScriptExpression {

  override val descriptorType: DescriptorType.SortedMulti.type =
    DescriptorType.SortedMulti

  override val scriptPubKey: MultiSignatureScriptPubKey = {
    MultiSignatureScriptPubKey(source.numSigsRequired, source.sortedPubKeys)
  }
  require(
    scriptPubKey.requiredSigs > 0,
    s"Must have positive requiredSigs in MultisigScriptExpression, got=${scriptPubKey.requiredSigs}")
  require(
    scriptPubKey.requiredSigs <= scriptPubKey.maxSigs,
    s"Required sigs greater than max sigs, got requiredSigs=${scriptPubKey.requiredSigs} maxSigs=${scriptPubKey.maxSigs}"
  )
}

case class ComboExpression(
    source: SingleKeyExpression,
    scriptType: ScriptType = ScriptType.PUBKEYHASH)
    extends ScriptExpression
    with KeyExpressionScriptExpression {
  override val descriptorType: DescriptorType = DescriptorType.Combo

  override val scriptPubKey: ScriptPubKey = {
    scriptType match {
      case ScriptType.PUBKEY             => P2PKScriptPubKey(source.pubKey)
      case ScriptType.PUBKEYHASH         => P2PKHScriptPubKey(source.pubKey)
      case ScriptType.WITNESS_V0_KEYHASH => P2WPKHWitnessSPKV0(source.pubKey)
      case ScriptType.SCRIPTHASH =>
        P2SHScriptPubKey(P2WPKHWitnessSPKV0(source.pubKey))
      case x @ (ScriptType.CLTV | ScriptType.CSV | ScriptType.MULTISIG |
          ScriptType.MULTISIG_WITH_TIMEOUT | ScriptType.NONSTANDARD |
          ScriptType.NONSTANDARD_IF_CONDITIONAL | ScriptType.NULLDATA |
          ScriptType.PUBKEY_WITH_TIMEOUT | ScriptType.WITNESS_V0_SCRIPTHASH |
          ScriptType.WITNESS_UNKNOWN | ScriptType.WITNESS_COMMITMENT |
          ScriptType.NOT_IF_CONDITIONAL | ScriptType.WITNESS_V1_TAPROOT) =>
        sys.error(s"Invalid ScripType for ComboExpression, got=$x")
    }
  }
}

object ScriptExpression extends StringFactory[ScriptExpression] {

  override def fromString(string: String): ScriptExpression = {
    val iter = DescriptorIterator(string)
    val descriptorType = iter.takeDescriptorType()
    val expression: ScriptExpression = descriptorType match {
      case DescriptorType.PKH =>
        P2PKHScriptExpression(iter.takeSingleKeyExpression())
      case DescriptorType.WPKH =>
        P2WPKHExpression(iter.takeSingleKeyExpression())
      case DescriptorType.WSH =>
        P2WSHExpression(iter.takeRawSPKScriptExpression())
      case DescriptorType.SH  => P2SHExpression(iter.takeScriptExpression())
      case DescriptorType.Raw => RawScriptExpression(iter.takeRawScriptPubKey())
      case DescriptorType.PK =>
        P2PKScriptExpression(iter.takeSingleKeyExpression())
      case DescriptorType.Multi =>
        MultisigExpression(iter.takeMultisigKeyExpression())
      case DescriptorType.SortedMulti =>
        SortedMultisigExpression(iter.takeMultisigKeyExpression())
      case DescriptorType.Combo =>
        ComboExpression(iter.takeSingleKeyExpression())
      case x @ (DescriptorType.TR) =>
        sys.error(
          s"Descriptor type not supported yet in ScriptExpression.fromString(), got=$x")
    }
    expression
  }
}

/** Tree expression corresponding to BIP386
  * https://github.com/bitcoin/bips/blob/master/bip-0386.mediawiki
  */
sealed abstract class TreeExpression extends ScriptExpression {
  override def descriptorType: DescriptorType.TR.type = DescriptorType.TR
  def xOnlyPubKey: XOnlyPubKey
  override def scriptPubKey: TaprootScriptPubKey
}

case class KeyPathOnlyTreeExpression(source: SingleKeyExpression)
    extends TreeExpression
    with KeyExpressionScriptExpression {

  override val xOnlyPubKey: XOnlyPubKey = source match {
    case x: XOnlyPublicKeyExpression => x.xOnlyPubKey
    case s: SingleKeyExpression      => s.pubKey.toXOnly
  }

  override val scriptPubKey: TaprootScriptPubKey =
    TaprootScriptPubKey.fromInternalKey(xOnlyPubKey)
}

case class ScriptPathTreeExpression(
    keyPath: KeyPathOnlyTreeExpression,
    source: TapscriptTreeExpression)
    extends TreeExpression
    with TapscriptTree {

  override def xOnlyPubKey: XOnlyPubKey = ???

  override def scriptPubKey: TaprootScriptPubKey = ???
}
