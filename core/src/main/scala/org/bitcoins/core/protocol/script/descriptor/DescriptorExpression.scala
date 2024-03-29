package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.protocol.script.{
  P2WPKHWitnessSPKV0,
  RawScriptPubKey,
  ScriptPubKey
}

sealed abstract class DescriptorExpression {}

sealed abstract class ScriptExpression extends DescriptorExpression {
  def scriptPubKey: ScriptPubKey

  def descriptorType: DescriptorType

  override def toString: String = {
    s"${descriptorType.toString}(${scriptPubKey.asmHex})"
  }
}

/** Denotes a key expression, examples of which are
  * 0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce600
  * [deadbeef/0h/0h/0h]0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce60
  * [deadbeef/0h/1h/2h]xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcE
  */
sealed abstract class KeyExpression extends DescriptorExpression

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
