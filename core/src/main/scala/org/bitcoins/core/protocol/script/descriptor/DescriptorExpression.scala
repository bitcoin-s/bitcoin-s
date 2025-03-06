package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{
  ECPrivateKeyUtil,
  ExtKey,
  ExtPrivateKey,
  ExtPublicKey
}
import org.bitcoins.core.hd.{BIP32Node, BIP32Path, HardenedType}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.*
import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.*

import scala.util.{Failure, Success}

sealed abstract class DescriptorExpression

/** Denotes a key expression, examples of which are
  * 0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce600
  * [deadbeef/0h/0h/0h]0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce60
  * [deadbeef/0h/1h/2h]xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcE
  */
sealed abstract class KeyExpression[T <: PublicKey]
    extends DescriptorExpression {
  this: PubKeyTypeExpression[T] =>
  def originOpt: Option[KeyOriginExpression]
}

sealed abstract class SingleKeyExpression[T <: PublicKey]
    extends KeyExpression[T] { this: PubKeyTypeExpression[T] =>
  def key: ECKeyBytes
}

sealed abstract class SingleECPublicKeyExpression
    extends SingleKeyExpression[ECPublicKey] {
  this: ECPublicKeyExpression =>

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
  this: XOnlyPublicKeyExpression =>
  def key: ECKeyBytes

  override def pubKey: XOnlyPubKey = key match {
    case priv: ECPrivateKeyBytes => priv.toPrivateKey.toXOnly
    case pub: ECPublicKeyBytes   => pub.toPublicKey.toXOnly
  }
}

/** A trait that allows us to parameterize by [[PublicKey]] type. This is needed
  * for re-using descriptors across [[ECPublicKey]] and [[XOnlyPubKey]]
  */
sealed trait PubKeyTypeExpression[T <: PublicKey]

sealed trait ECPublicKeyExpression extends PubKeyTypeExpression[ECPublicKey] {
  this: KeyExpression[ECPublicKey] =>
  def pubKey: ECPublicKey
}

sealed trait XOnlyPublicKeyExpression
    extends PubKeyTypeExpression[XOnlyPubKey] {
  this: KeyExpression[XOnlyPubKey] =>
  def pubKey: XOnlyPubKey
}

sealed abstract class PrivateECPublicKeyExpression
    extends SingleECPublicKeyExpression {
  this: ECPublicKeyExpression =>
  override def key: ECPrivateKeyBytes
}

sealed abstract class PrivateXOnlyPublicKeyExpression
    extends SingleXOnlyPubKeyExpression {
  this: XOnlyPublicKeyExpression =>
  override def key: ECPrivateKeyBytes
}

/** A private key descriptor expression Examples of what this data structure can
  * represent 5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss
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

/** A private key expression that produces an [[XOnlyPubKey]] Example:
  * tr(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)
  * @param raw
  */
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
  this: ECPublicKeyExpression =>
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

/** A single [[XOnlyPubKey]] in a descriptor Example:
  * tr(a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)
  */
case class RawPublicXOnlyPublicKeyExpression(
    originOpt: Option[KeyOriginExpression],
    override val pubKey: XOnlyPubKey)
    extends SingleXOnlyPubKeyExpression
    with XOnlyPublicKeyExpression {

  override def key: ECKeyBytes = pubKey.publicKey.toPublicKeyBytes()

  override def toString(): String = {
    originOpt.map(_.toString).getOrElse("") + pubKey.hex
  }
}

/** Represents key expressions that are BIP32 keys Examples:
  * xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc
  * [deadbeef/0'/1'/2']xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc/3/4/5
  * [deadbeef/0'/1'/2']xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc/3/4/5/\*
  */
sealed abstract class ExtECPublicKeyExpression
    extends SingleECPublicKeyExpression {
  this: ECPublicKeyExpression =>
  def extKey: ExtKey

  def pathOpt: Option[BIP32Path]

  /** Outer Option represents if we use this key or derive children Inner option
    * represents whether child keys are hardened or not if they are hardedned,
    * return the specifi [[HardenedType]]
    */
  def childrenHardenedOpt: Option[Option[HardenedType]]

  def deriveChild(idx: Int): BaseECKey

  override def toString(): String = {
    val hardenedStr: String = childrenHardenedOpt match {
      case Some(Some(h)) => s"/*${h.toString}"
      case Some(None)    => "/*"
      case None          => ""
    }
    originOpt.map(_.toString).getOrElse("") +
      ExtKey.toString(extKey) +
      pathOpt.map(_.toString.drop(1)).getOrElse("") +
      hardenedStr

  }
}

sealed abstract class ExtXOnlyPublicKeyExpression
    extends SingleXOnlyPubKeyExpression { this: XOnlyPublicKeyExpression =>

  /** Since implementations are so similar, just piggy back off of the
    * ExtECPublicKeyExpression implementation rather than duplicating everything
    */
  def ecPublicKeyExpression: ExtECPublicKeyExpression

  def extKey: ExtKey = ecPublicKeyExpression.extKey

  def pathOpt: Option[BIP32Path] = ecPublicKeyExpression.pathOpt

  def childrenHardenedOpt: Option[Option[HardenedType]] =
    ecPublicKeyExpression.childrenHardenedOpt

  def deriveChild(idx: Int): BaseECKey = ecPublicKeyExpression.deriveChild(idx)

  override def key: ECKeyBytes = ecPublicKeyExpression.key

  override def toString(): String = {
    ecPublicKeyExpression.toString()
  }
}

/** Produces [[ECPublicKey]] from [[ExtPrivateKey]] */
case class XprvECPublicKeyExpression(
    override val extKey: ExtPrivateKey,
    originOpt: Option[KeyOriginExpression],
    pathOpt: Option[BIP32Path],
    childrenHardenedOpt: Option[Option[HardenedType]])
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
      BIP32Node(index = idx, hardenedOpt = childrenHardenedOpt.get)
    val fullPath: BIP32Path = pathOpt match {
      case Some(p) => BIP32Path(p.path.appended(node))
      case None    => BIP32Path(node)
    }
    extKey.deriveChildPrivKey(fullPath).key
  }
}

case class XprvXOnlyPublicKeyExpression(
    ecPublicKeyExpression: XprvECPublicKeyExpression)
    extends ExtXOnlyPublicKeyExpression
    with XOnlyPublicKeyExpression {

  override val originOpt: Option[KeyOriginExpression] =
    ecPublicKeyExpression.originOpt
}

/** Produces [[ECPublicKey]] from [[ExtPublicKey]]
  * @param extKey
  * @param originOpt
  * @param pathOpt
  * @param childrenHardenedOpt
  */
case class XpubECPublicKeyExpression(
    override val extKey: ExtPublicKey,
    originOpt: Option[KeyOriginExpression],
    pathOpt: Option[BIP32Path],
    childrenHardenedOpt: Option[Option[HardenedType]])
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
    val node = BIP32Node(index = idx, hardenedOpt = childrenHardenedOpt.get)
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

/** Produces [[XOnlyPubKey]] from [[ExtPublicKey]]
  * @param ecPublicKeyExpression
  */
case class XpubXOnlyPublicKeyExpression(
    ecPublicKeyExpression: ExtECPublicKeyExpression)
    extends ExtXOnlyPublicKeyExpression
    with XOnlyPublicKeyExpression {

  override val originOpt: Option[KeyOriginExpression] =
    ecPublicKeyExpression.originOpt
}

case class MultisigKeyExpression(
    numSigsRequired: Int,
    keyExpressions: Vector[SingleECPublicKeyExpression])
    extends KeyExpression[ECPublicKey]
    // cannot directly mixin ECPublicKeyExpression
    // because we don't have a single pubKey to represent multisig
    with PubKeyTypeExpression[ECPublicKey] {
  override val originOpt: None.type = None

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
  * pk(xprvA2JDeKCSNNZky6uBCviVfJSKyQ1mDYahRjijr5idH2WwLsEd4Hsb2Tyh8RfQMuPh7f7RtyzTtdrbdqqsunu5Mm3wDvUAKRHSC34sJ7in334/0),
  * { {
  * pk(xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL),
  * pk(02df12b7035bdac8e3bab862a3a83d06ea6b17b6753d52edecba9be46f5d09e076)},pk(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)
  * } }
  * @param scriptExpressions
  */
sealed abstract class TapscriptTreeExpression extends DescriptorExpression {
  def leafs: Vector[TapscriptLeafExpression]

  def tree: TapscriptTree
}

/** Branches in a [[TapscriptTreeExpression]]. This corresponds to [[TapBranch]]
  */
case class TapscriptBranchExpression(
    left: TapscriptTreeExpression,
    right: TapscriptTreeExpression)
    extends TapscriptTreeExpression {

  def leafs: Vector[TapscriptLeafExpression] = {
    left.leafs ++ right.leafs
  }

  override def tree: TapBranch = TapBranch(left.tree, right.tree)

  override def toString(): String = {
    s"{${left.toString},${right.toString}}"
  }
}

/** A leaf in a [[TapscriptTreeExpression]]. This corresponds to [[TapLeaf]] */
case class TapscriptLeafExpression(source: RawSPKScriptExpression)
    extends TapscriptTreeExpression {
  override def leafs: Vector[TapscriptLeafExpression] = Vector(this)

  override def toString: String = {
    s"${source.toString}"
  }

  private def scriptPubKey: ScriptPubKey = source.scriptPubKey

  override def tree: TapLeaf =
    TapLeaf(LeafVersion.Tapscript, scriptPubKey)
}

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
        // cannot convert correctly, so just re-parse
        val iter = DescriptorIterator(string)
        iter.takeInternalPublicKeyExpression()
    }
  }
}

object MultisigKeyExpression extends StringFactory[MultisigKeyExpression] {

  override def fromString(string: String): MultisigKeyExpression = {
    val (requiredSigsStr, keyExpressionsStr) = string.span(_ != ',')

    val split = keyExpressionsStr
      .drop(1) // drop ','
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

/** A descriptor that produces a scriptPubKey of type [[RawScriptPubKey]]
  */
sealed abstract class RawSPKScriptExpression extends ScriptExpression {
  override def scriptPubKey: RawScriptPubKey
}

sealed abstract class MultisigScriptExpression
    extends RawSPKScriptExpression
    with KeyExpressionScriptExpression[ECPublicKey] {
  override def scriptPubKey: MultiSignatureScriptPubKey

  override def source: MultisigKeyExpression

  def isSorted: Boolean = descriptorType == ScriptDescriptorType.SortedMulti

}

/** The source for where a [[ScriptExpression]] derives its information */
sealed trait ExpressionSource { this: ScriptExpression =>
  def source: DescriptorExpression

  override def toString: String = {
    s"${descriptorType.toString}(${source.toString})"
  }
}

/** A script expression derived from a key expression Example:
  * tr(a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)
  * tr(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)
  * pkh([bd16bee5/2147483647']xpub69H7F5dQzmVd3vPuLKtcXJziMEQByuDidnX3YdwgtNsecY5HRGtAAQC5mXTt4dsv9RzyjgDjAQs9VGVV6ydYCHnprc9vvaA5YtqWyL6hyds/0)
  */
sealed trait KeyExpressionScriptExpression[T <: PublicKey]
    extends ExpressionSource {
  this: ScriptExpression =>
  override def source: KeyExpression[T]
}

/** A script expression nested inside of another script expression Example:
  * sh(wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)))
  */
sealed trait NestedScriptExpression extends ExpressionSource {
  this: ScriptExpression =>
  override def source: ScriptExpression
}

sealed trait TapscriptTreeExpressionSource extends ExpressionSource {
  this: TreeExpression =>
  override def source: TapscriptTreeExpression
}

/** Examples: raw(deadbeef) raw(a9149a4d9901d6af519b2a23d4a2f51650fcba87ce7b87)
  */
case class RawScriptExpression(scriptPubKey: RawScriptPubKey)
    extends RawSPKScriptExpression {
  override val descriptorType: ScriptDescriptorType.Raw.type =
    ScriptDescriptorType.Raw

  override def toString: String = {
    s"${descriptorType.toString}(${scriptPubKey.asmHex})"
  }
}

case class P2PKHScriptExpression(source: SingleECPublicKeyExpression)
    extends RawSPKScriptExpression
    with KeyExpressionScriptExpression[ECPublicKey] {
  override val descriptorType: ScriptDescriptorType.PKH.type =
    ScriptDescriptorType.PKH

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
  override val descriptorType: ScriptDescriptorType.PK.type =
    ScriptDescriptorType.PK

  override val scriptPubKey: P2PKScriptPubKey = {
    val pub = source match {
      case ec: SingleECPublicKeyExpression =>
        ec.key match {
          case priv: ECPrivateKeyBytes =>
            priv.publicKeyBytes.toPublicKey
          case pub: ECPublicKeyBytes => pub.toPublicKey
        }
      case x: SingleXOnlyPubKeyExpression =>
        x.pubKey
    }
    P2PKScriptPubKey(pub)
  }
}

case class P2WPKHExpression(source: SingleECPublicKeyExpression)
    extends ScriptExpression
    with KeyExpressionScriptExpression[ECPublicKey] {
  override val descriptorType: ScriptDescriptorType.WPKH.type =
    ScriptDescriptorType.WPKH

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
  override val descriptorType: ScriptDescriptorType.WSH.type =
    ScriptDescriptorType.WSH

  override val scriptPubKey: P2WSHWitnessSPKV0 = {
    P2WSHWitnessSPKV0(source.scriptPubKey)
  }
}

case class P2SHExpression(source: ScriptExpression)
    extends ScriptExpression
    with NestedScriptExpression {
  override val descriptorType: ScriptDescriptorType.SH.type =
    ScriptDescriptorType.SH

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

/** A multisig expression Example:
  * multi(1,L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1,5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss)
  * @param source
  */
case class MultisigExpression(source: MultisigKeyExpression)
    extends MultisigScriptExpression
    with KeyExpressionScriptExpression[ECPublicKey] {
  override val descriptorType: ScriptDescriptorType.Multi.type =
    ScriptDescriptorType.Multi

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

/** Multisig expressions with lexographically sorted public keys Example:
  * sortedmulti(2,xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL/\*,xpub68NZiKmJWnxxS6aaHmn81bvJeTESw724CRDs6HbuccFQN9Ku14VQrADWgqbhhTHBaohPX4CjNLf9fq9MYo6oDaPPLPxSb7gwQN3ih19Zm4Y/0/0/\*)
  */
case class SortedMultisigExpression(source: MultisigKeyExpression)
    extends MultisigScriptExpression
    with KeyExpressionScriptExpression[ECPublicKey] {

  override val descriptorType: ScriptDescriptorType.SortedMulti.type =
    ScriptDescriptorType.SortedMulti

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

/** An expression that can produce multiple types of scripts Example:
  * combo(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)
  * combo([01234567]xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL)
  * @param source
  * @param scriptType
  */
case class ComboExpression(
    source: SingleECPublicKeyExpression,
    scriptType: ScriptType = ScriptType.PUBKEYHASH)
    extends ScriptExpression
    with KeyExpressionScriptExpression[ECPublicKey] {
  override val descriptorType: DescriptorType = ScriptDescriptorType.Combo

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
    val descriptorType = iter.takeScriptDescriptorType()
    val expression: ScriptExpression = descriptorType match {
      case ScriptDescriptorType.PKH =>
        P2PKHScriptExpression(iter.takeSingleECKeyExpression())
      case ScriptDescriptorType.WPKH =>
        P2WPKHExpression(iter.takeSingleECKeyExpression())
      case ScriptDescriptorType.WSH =>
        P2WSHExpression(iter.takeRawSPKScriptExpression())
      case ScriptDescriptorType.SH =>
        P2SHExpression(iter.takeScriptExpressionECKey())
      case ScriptDescriptorType.Raw =>
        RawScriptExpression(iter.takeRawScriptPubKey())
      case ScriptDescriptorType.PK =>
        P2PKScriptExpression(iter.takeSingleECKeyExpression())
      case ScriptDescriptorType.Multi =>
        MultisigExpression(iter.takeMultisigKeyExpression())
      case ScriptDescriptorType.SortedMulti =>
        SortedMultisigExpression(iter.takeMultisigKeyExpression())
      case ScriptDescriptorType.Combo =>
        ComboExpression(iter.takeSingleECKeyExpression())
      case ScriptDescriptorType.TR =>
        sys.error(
          s"Cannot create tapscript expression's with ECPublicKey, got=$string")
    }
    expression
  }
}

object ScriptExpressionXOnlyKey extends StringFactory[ScriptExpression] {

  override def fromString(string: String): ScriptExpression = {
    val iter = DescriptorIterator(string)
    val descriptorType = iter.takeScriptDescriptorType()
    val expression: ScriptExpression = descriptorType match {
      case ScriptDescriptorType.Raw =>
        RawScriptExpression(iter.takeRawScriptPubKey())
      case ScriptDescriptorType.PK =>
        P2PKScriptExpression(iter.takeSingleXOnlyPubKeyExpression())
      case ScriptDescriptorType.Multi =>
        MultisigExpression(iter.takeMultisigKeyExpression())
      case ScriptDescriptorType.SortedMulti =>
        SortedMultisigExpression(iter.takeMultisigKeyExpression())
      case x @ (ScriptDescriptorType.Combo | ScriptDescriptorType.TR |
          ScriptDescriptorType.SH | ScriptDescriptorType.WSH |
          ScriptDescriptorType.WPKH | ScriptDescriptorType.PKH) =>
        sys.error(
          s"Cannot create tapscript descriptor for descriptorType=$x, got=$string")
    }
    expression
  }
}

/** Tree expression corresponding to BIP386
  * https://github.com/bitcoin/bips/blob/master/bip-0386.mediawiki
  */
sealed abstract class TreeExpression extends ScriptExpression {
  override def descriptorType: ScriptDescriptorType.TR.type =
    ScriptDescriptorType.TR
  override def scriptPubKey: TaprootScriptPubKey
}

/** A tapscript tree expression with ONLY the keypath. Example:
  * tr(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)
  * tr(a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)
  */
case class KeyPathOnlyTreeExpression(source: SingleXOnlyPubKeyExpression)
    extends TreeExpression
    with KeyExpressionScriptExpression[XOnlyPubKey] {

  override val scriptPubKey: TaprootScriptPubKey =
    TaprootScriptPubKey.fromInternalKey(source.pubKey)
}

/** Tapscript tree with BOTH keypath and script path cases. Example:
  * tr(a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd,pk(669b8afcec803a0d323e9a17f3ea8e68e8abe5a278020a929adbec52421adbd0))
  */
case class ScriptPathTreeExpression(
    keyPath: KeyPathOnlyTreeExpression,
    source: TapscriptTreeExpression)
    extends TreeExpression
    with TapscriptTreeExpressionSource {

  override def scriptPubKey: TaprootScriptPubKey = {
    val tree = source.tree
    val (_, spk) = TaprootScriptPubKey
      .fromInternalKeyTapscriptTree(keyPath.source.pubKey, tree)
    spk
  }

  override def toString(): String = {
    s"${descriptorType.toString}(${keyPath.source.toString},${source.toString()})"
  }
}

case class AddressExpression(address: BitcoinAddress)
    extends DescriptorExpression {
  override def toString: String = {
    s"${DescriptorType.Addr.toString}(${address.toString})"
  }
}
