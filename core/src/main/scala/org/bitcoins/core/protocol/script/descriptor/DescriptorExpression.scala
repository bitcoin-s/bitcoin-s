package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{ECPrivateKeyUtil, ExtKey}
import org.bitcoins.core.protocol.script.{
  P2WPKHWitnessSPKV0,
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

object KeyExpression extends StringFactory[KeyExpression] {

  override def fromString(string: String): KeyExpression = {
    val iter = DescriptorIterator(string)
    val keyOriginOpt = iter.takeKeyOriginOpt()
    val isExtKey = ExtKey.prefixes.exists(p => iter.current.startsWith(p))
    if (isExtKey) {
      val _ = iter.takeExtKey()
      sys.error(s"ExtKeys not supported yet")
    } else {
      val cp =
        iter.current // needed to parse network info in case of WIF private key
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

case class P2WPKHExpression(xPubHDPath: XPubHDPath) extends ScriptExpression {
  override val descriptorType: DescriptorType.WPKH.type = DescriptorType.WPKH
  val xpub = xPubHDPath.xpub
  val hdPath = xPubHDPath.bip32Path

  override val scriptPubKey: P2WPKHWitnessSPKV0 = {
    val pubKey = xpub.deriveChildPubKey(hdPath).get.key
    P2WPKHWitnessSPKV0(pubKey)
  }
}
