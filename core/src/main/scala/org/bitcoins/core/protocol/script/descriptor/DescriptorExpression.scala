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
sealed abstract class KeyExpression[T <: PublicKey]
    extends DescriptorExpression {
  _: PubKeyTypeExpression[T] =>
  def originOpt: Option[KeyOriginExpression]
}

sealed abstract class SingleKeyExpression[T <: PublicKey]
    extends KeyExpression[T] { _: PubKeyTypeExpression[T] =>
  def key: ECKeyBytes
}

sealed abstract class SingleECPublicKeyExpression
    extends SingleKeyExpression[ECPublicKey] {
  _: ECPublicKeyExpression =>

  override def pubKey: ECPublicKey = key match {
    case priv: ECPrivateKeyBytes => priv.publicKeyBytes.toPublicKey
    case pub: ECPublicKeyBytes   => pub.toPublicKey
  }
}

/** Represents an expression that corresponds to a single [[XOnlyPubKey]]
  * Example:
  * tr(a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)
  */
sealed abstract class SingleXOnlyPubKeyExpression
    extends SingleKeyExpression[XOnlyPubKey] {
  _: XOnlyPublicKeyExpression =>
  def key: ECKeyBytes

  override def pubKey: XOnlyPubKey = key match {
    case priv: ECPrivateKeyBytes => priv.toPrivateKey.toXOnly
    case pub: ECPublicKeyBytes   => pub.toPublicKey.toXOnly
  }
}

/** A trait that allows us to parameterize by [[PublicKey]] type.
  * This is needed for re-using descriptors across [[ECPublicKey]] and [[XOnlyPubKey]]
  */
sealed trait PubKeyTypeExpression[T <: PublicKey]

sealed trait ECPublicKeyExpression extends PubKeyTypeExpression[ECPublicKey] {
  _: KeyExpression[ECPublicKey] =>
  def pubKey: ECPublicKey
}

sealed trait XOnlyPublicKeyExpression
    extends PubKeyTypeExpression[XOnlyPubKey] {
  _: KeyExpression[XOnlyPubKey] =>
  def pubKey: XOnlyPubKey
}

sealed abstract class PrivateECPublicKeyExpression
    extends SingleECPublicKeyExpression {
  _: ECPublicKeyExpression =>
  override def key: ECPrivateKeyBytes
}

sealed abstract class PrivateXOnlyPublicKeyExpression
    extends SingleXOnlyPubKeyExpression {
  _: XOnlyPublicKeyExpression =>
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
case class RawPrivateECPublicKeyExpression(
    key: ECPrivateKeyBytes,
    network: NetworkParameters,
    originOpt: Option[KeyOriginExpression])
    extends PrivateECPublicKeyExpression
    with ECPublicKeyExpression {

  override def toString(): String = {
    originOpt.map(_.toString).getOrElse("") +
      ECPrivateKeyUtil.toWIF(key, network)
  }
}

case class RawPrivateXOnlyPublicKeyExpression(
    raw: RawPrivateECPublicKeyExpression)
    extends PrivateXOnlyPublicKeyExpression
    with XOnlyPublicKeyExpression {
  override val originOpt: Option[KeyOriginExpression] = raw.originOpt
  override val key: ECPrivateKeyBytes = raw.key
  override def toString(): String = raw.toString()
}

sealed abstract class PublicECPublicKeyExpression
    extends SingleECPublicKeyExpression {
  _: ECPublicKeyExpression =>
  override def key: ECPublicKeyBytes
}

/** A key expression that looks like
  * 0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce600
  * [deadbeef/0h/0h/0h]0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce600
  * @param bytes
  * @param originOpt
  */
case class RawPublicECPublicKeyExpression(
    key: ECPublicKeyBytes,
    originOpt: Option[KeyOriginExpression])
    extends PublicECPublicKeyExpression
    with ECPublicKeyExpression {

  override def toString(): String = {
    originOpt.map(_.toString).getOrElse("") +
      key.hex
  }
}

case class InternalPublicKeyExpression(override val pubKey: XOnlyPubKey)
    extends SingleXOnlyPubKeyExpression
    with XOnlyPublicKeyExpression {
  override val originOpt: Option[KeyOriginExpression] = None

  override def key: ECKeyBytes = pubKey.publicKey.toPublicKeyBytes()

  override def toString(): String = {
    originOpt.map(_.toString).getOrElse("") + pubKey.hex
  }

}

/** Represents key expressions that are BIP32 keys
  * Examples:
  * xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc
  * [deadbeef/0'/1'/2']xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc/3/4/5
  * [deadbeef/0'/1'/2']xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc/3/4/5/\*
  */
sealed abstract class ExtECPublicKeyExpression
    extends SingleECPublicKeyExpression {
  _: ECPublicKeyExpression =>
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

sealed abstract class ExtXOnlyPublicKeyExpression
    extends SingleXOnlyPubKeyExpression { _: XOnlyPublicKeyExpression =>

  /** Since implementations are so similar, just piggy back off of the ExtECPublicKeyExpression
    * implementation rather than duplicating everything
    */
  protected def ecPublicKeyExpression: ExtECPublicKeyExpression

  def extKey: ExtKey = ecPublicKeyExpression.extKey

  def pathOpt: Option[BIP32Path] = ecPublicKeyExpression.pathOpt

  def childrenHardenedOpt: Option[Boolean] =
    ecPublicKeyExpression.childrenHardenedOpt

  def deriveChild(idx: Int): BaseECKey = ecPublicKeyExpression.deriveChild(idx)

  override def key: ECKeyBytes = ecPublicKeyExpression.key

  override def toString(): String = {
    ecPublicKeyExpression.toString()
  }
}

case class XprvECPublicKeyExpression(
    override val extKey: ExtPrivateKey,
    originOpt: Option[KeyOriginExpression],
    pathOpt: Option[BIP32Path],
    childrenHardenedOpt: Option[Boolean])
    extends ExtECPublicKeyExpression
    with ECPublicKeyExpression {

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

case class XprvXOnlyPublicKeyExpression(
    ecPublicKeyExpression: ExtECPublicKeyExpression)
    extends ExtXOnlyPublicKeyExpression
    with XOnlyPublicKeyExpression {
  override val originOpt: Option[KeyOriginExpression] = None
}

case class XpubECPublicKeyExpression(
    override val extKey: ExtPublicKey,
    originOpt: Option[KeyOriginExpression],
    pathOpt: Option[BIP32Path],
    childrenHardenedOpt: Option[Boolean])
    extends ExtECPublicKeyExpression
    with ECPublicKeyExpression {

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

case class XpubXOnlyPublicKeyExpression(
    ecPublicKeyExpression: ExtECPublicKeyExpression)
    extends ExtXOnlyPublicKeyExpression
    with XOnlyPublicKeyExpression {
  override val originOpt: Option[KeyOriginExpression] = None
}

case class MultisigKeyExpression(
    numSigsRequired: Int,
    keyExpressions: Vector[SingleECPublicKeyExpression])
    extends KeyExpression[ECPublicKey]
    //cannot directly mixin ECPublicKeyExpression
    //because we don't have a single pubKey to represent multisig
    with PubKeyTypeExpression[ECPublicKey] {
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

object SingleECPublicKeyExpression
    extends StringFactory[SingleECPublicKeyExpression] {

  override def fromString(string: String): SingleECPublicKeyExpression = {
    val iter = DescriptorIterator(string)
    val keyOriginOpt = iter.takeKeyOriginOpt()
    val isExtKey = ExtKey.prefixes.exists(p => iter.current.startsWith(p))
    if (isExtKey) {
      val extKey = iter.takeExtKey()
      val pathOpt = iter.takeBIP32PathOpt()
      val childrenHardenedOpt = iter.takeChildrenHardenedOpt()
      extKey match {
        case xprv: ExtPrivateKey =>
          XprvECPublicKeyExpression(xprv,
                                    keyOriginOpt,
                                    pathOpt,
                                    childrenHardenedOpt)
        case xpub: ExtPublicKey =>
          XpubECPublicKeyExpression(xpub,
                                    keyOriginOpt,
                                    pathOpt,
                                    childrenHardenedOpt)
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
              RawPrivateECPublicKeyExpression(key = priv,
                                              network = network,
                                              originOpt = keyOriginOpt)
            case Failure(err) => throw err
          }
        case pub: ECPublicKeyBytes =>
          RawPublicECPublicKeyExpression(key = pub, originOpt = keyOriginOpt)
      }
    }
  }
}

object SingleXOnlyPubKeyExpression
    extends StringFactory[SingleXOnlyPubKeyExpression] {

  override def fromString(string: String): SingleXOnlyPubKeyExpression = {
    SingleECPublicKeyExpression.fromString(string) match {
      case xprv: XprvECPublicKeyExpression => XprvXOnlyPublicKeyExpression(xprv)
      case xpub: XpubECPublicKeyExpression => XpubXOnlyPublicKeyExpression(xpub)
      case rawPriv: RawPrivateECPublicKeyExpression =>
        RawPrivateXOnlyPublicKeyExpression(rawPriv)
      case _: RawPublicECPublicKeyExpression =>
        //cannot convert correctly, so just re-parse
        val iter = DescriptorIterator(string)
        iter.takeInternalPublicKeyExpression()
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
    val keyExpressions = split.map(SingleECPublicKeyExpression.fromString(_))
    MultisigKeyExpression(requiredSigsStr.toInt, keyExpressions)
  }
}

object KeyExpression extends StringFactory[KeyExpression[ECPublicKey]] {

  override def fromString(string: String): KeyExpression[ECPublicKey] = {
    MultisigKeyExpression
      .fromStringOpt(string)
      .getOrElse(SingleECPublicKeyExpression.fromString(string))
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
    with KeyExpressionScriptExpression[ECPublicKey] {
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
sealed trait KeyExpressionScriptExpression[T <: PublicKey]
    extends ExpressionSource {
  _: ScriptExpression =>
  override def source: KeyExpression[T]
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

case class P2PKHScriptExpression(source: SingleECPublicKeyExpression)
    extends RawSPKScriptExpression
    with KeyExpressionScriptExpression[ECPublicKey] {
  override val descriptorType: DescriptorType.PKH.type = DescriptorType.PKH

  override val scriptPubKey: P2PKHScriptPubKey = {
    val pub = source.key match {
      case priv: ECPrivateKeyBytes => priv.publicKeyBytes.toPublicKey
      case pub: ECPublicKeyBytes   => pub.toPublicKey
    }
    P2PKHScriptPubKey(pub)
  }
}

case class P2PKScriptExpression[T <: PublicKey](source: SingleKeyExpression[T])
    extends RawSPKScriptExpression
    with KeyExpressionScriptExpression[T] {
  override val descriptorType: DescriptorType.PK.type = DescriptorType.PK

  override val scriptPubKey: P2PKScriptPubKey = {
    val pub = source match {
      case ec: SingleECPublicKeyExpression =>
        ec.key match {
          case priv: ECPrivateKeyBytes =>
            priv.publicKeyBytes.toPublicKey
          case pub: ECPublicKeyBytes => pub.toPublicKey
        }
      case x: SingleXOnlyPubKeyExpression =>
        x.pubKey.publicKey //is this right?
    }
    P2PKScriptPubKey(pub)
  }
}

case class P2WPKHExpression(source: SingleECPublicKeyExpression)
    extends ScriptExpression
    with KeyExpressionScriptExpression[ECPublicKey] {
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
    with KeyExpressionScriptExpression[ECPublicKey] {
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
    with KeyExpressionScriptExpression[ECPublicKey] {

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
    source: SingleECPublicKeyExpression,
    scriptType: ScriptType = ScriptType.PUBKEYHASH)
    extends ScriptExpression
    with KeyExpressionScriptExpression[ECPublicKey] {
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

/** Creates [[ScriptExpression]] from [[ECPublicKey]]'s */
object ScriptExpressionECKey extends StringFactory[ScriptExpression] {

  override def fromString(string: String): ScriptExpression = {
    val iter = DescriptorIterator(string)
    val descriptorType = iter.takeDescriptorType()
    val expression: ScriptExpression = descriptorType match {
      case DescriptorType.PKH =>
        P2PKHScriptExpression(iter.takeSingleECKeyExpression())
      case DescriptorType.WPKH =>
        P2WPKHExpression(iter.takeSingleECKeyExpression())
      case DescriptorType.WSH =>
        P2WSHExpression(iter.takeRawSPKScriptExpression())
      case DescriptorType.SH  => P2SHExpression(iter.takeScriptExpressionECKey())
      case DescriptorType.Raw => RawScriptExpression(iter.takeRawScriptPubKey())
      case DescriptorType.PK =>
        P2PKScriptExpression(iter.takeSingleECKeyExpression())
      case DescriptorType.Multi =>
        MultisigExpression(iter.takeMultisigKeyExpression())
      case DescriptorType.SortedMulti =>
        SortedMultisigExpression(iter.takeMultisigKeyExpression())
      case DescriptorType.Combo =>
        ComboExpression(iter.takeSingleECKeyExpression())
      case DescriptorType.TR =>
        sys.error(
          s"Cannot create tapscript expression's with ECPublicKey, got=$string")
    }
    expression
  }
}

object ScriptExpressionXOnlyKey extends StringFactory[ScriptExpression] {

  override def fromString(string: String): ScriptExpression = {
    val iter = DescriptorIterator(string)
    val descriptorType = iter.takeDescriptorType()
    val expression: ScriptExpression = descriptorType match {
      case DescriptorType.PKH =>
        //P2PKHScriptExpression(iter.takeSingleXOnlyPubKeyExpression())
        ???
      case DescriptorType.WPKH =>
        //P2WPKHExpression(iter.takeSingleXOnlyPubKeyExpression())
        ???
      case DescriptorType.WSH =>
        //P2WSHExpression(iter.takeRawSPKScriptExpression())
        ???
      case DescriptorType.SH =>
        //P2SHExpression(iter.takeSingleXOnlyPubKeyExpression())
        ???
      case DescriptorType.Raw => RawScriptExpression(iter.takeRawScriptPubKey())
      case DescriptorType.PK =>
        P2PKScriptExpression(iter.takeSingleXOnlyPubKeyExpression())
      case DescriptorType.Multi =>
        MultisigExpression(iter.takeMultisigKeyExpression())
      case DescriptorType.SortedMulti =>
        SortedMultisigExpression(iter.takeMultisigKeyExpression())
      case DescriptorType.Combo =>
        //ComboExpression(iter.takeSingleXOnlyPubKeyExpression())
        ???
      case DescriptorType.TR =>
        sys.error(
          s"Cannot create tapscript expression's with ECPublicKey, got=$string")
    }
    expression
  }
}

/** Tree expression corresponding to BIP386
  * https://github.com/bitcoin/bips/blob/master/bip-0386.mediawiki
  */
sealed abstract class TreeExpression extends ScriptExpression {
  override def descriptorType: DescriptorType.TR.type = DescriptorType.TR
  override def scriptPubKey: TaprootScriptPubKey
}

case class KeyPathOnlyTreeExpression(source: SingleXOnlyPubKeyExpression)
    extends TreeExpression
    with KeyExpressionScriptExpression[XOnlyPubKey] {

  override val scriptPubKey: TaprootScriptPubKey =
    TaprootScriptPubKey.fromInternalKey(source.pubKey)
}

case class ScriptPathTreeExpression(
    keyPath: KeyPathOnlyTreeExpression,
    source: TapscriptTreeExpression)
    extends TreeExpression
    with TapscriptTree {

  override def scriptPubKey: TaprootScriptPubKey = {
    ???
  }
}
