package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.protocol.script.{
  P2WPKHWitnessSPKV0,
  RawScriptPubKey,
  ScriptPubKey
}

sealed abstract class DescriptorExpression {
  def descriptorType: DescriptorType

}

sealed abstract class ScriptExpression extends DescriptorExpression {
  def scriptPubKey: ScriptPubKey

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
