package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.{ECDigitalSignature, ECPublicKey}
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import org.bitcoins.core.util.{BitcoinSUtil, Factory}

/**
  * Created by chris on 11/10/16.
  * The witness used to evaluate a [[ScriptPubKey]] inside of Bitcoin
  * [[https://github.com/bitcoin/bitcoin/blob/57b34599b2deb179ff1bd97ffeab91ec9f904d85/src/script/script.h#L648-L660]]
  */
sealed trait ScriptWitness extends NetworkElement {

  /** The byte vectors that are placed on to the stack when evaluating a witness program */
  def stack : Seq[Seq[Byte]]

  override def toString = "ScriptWitness(" + stack.map(BitcoinSUtil.encodeHex(_)).toString + ")"

  override def bytes = RawScriptWitnessParser.write(this)
}

case object EmptyScriptWitness extends ScriptWitness {
  override def stack = Nil

  override def bytes = Seq(0.toByte)
}

object ScriptWitness extends Factory[ScriptWitness] {
  private case class ScriptWitnessImpl(stack: Seq[Seq[Byte]]) extends ScriptWitness

  def apply(stack: Seq[Seq[Byte]]): ScriptWitness = ScriptWitnessImpl(stack)

  override def fromBytes(bytes: Seq[Byte]): ScriptWitness = RawScriptWitnessParser.read(bytes)

  def apply(signature: ECDigitalSignature, publicKey: ECPublicKey): ScriptWitness = {
    val sigConstant = signature.bytes
    val pubKeyConstant = publicKey.bytes
    ScriptWitness(Seq(sigConstant, pubKeyConstant))
  }
}