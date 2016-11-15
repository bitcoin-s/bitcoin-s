package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.{ECDigitalSignature, ECPublicKey}
import org.bitcoins.core.script.constant.{ScriptConstant, ScriptToken}

/**
  * Created by chris on 11/10/16.
  */
sealed trait ScriptWitness {

  /** The [[ScriptToken]]s that are placed on to the stack when evaluating a witness program */
  def stack : Seq[ScriptToken]
}

object ScriptWitness {
  private case class ScriptWitnessImpl(stack: Seq[ScriptToken]) extends ScriptWitness

  def apply(stack: Seq[ScriptToken]): ScriptWitness = ScriptWitnessImpl(stack)

  def apply(signature: ECDigitalSignature, publicKey: ECPublicKey): ScriptWitness = {
    val sigConstant = ScriptConstant(signature.bytes)
    val pubKeyConstant = ScriptConstant(publicKey.bytes)
    ScriptWitness(Seq(sigConstant, pubKeyConstant))
  }
}