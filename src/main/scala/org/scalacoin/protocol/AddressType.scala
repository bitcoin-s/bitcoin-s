package org.scalacoin.protocol

/**
 * Created by chrids s on 1/12/16.
 */
sealed trait ScriptType

case object P2PKH extends ScriptType {
  override def toString = "pubkeyhash"
}

case object P2SH extends ScriptType {
  override def toString = "scripthash"
}

case object MultiSignature extends ScriptType  {
  override def toString = "multisignature"
}

case object NonStandard extends ScriptType {
  override def toString = "non-standard"
}

