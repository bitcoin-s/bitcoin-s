package org.bitcoins.core.protocol.script

import org.bitcoins.core.script.constant.ScriptToken

/**
  * Created by chris on 11/10/16.
  */
sealed trait ScriptWitness {
  def asm : Seq[ScriptToken]
}

object ScriptWitness {
  private case class ScriptWitnessImpl(asm: Seq[ScriptToken]) extends ScriptWitness

  def apply(asm: Seq[ScriptToken]): ScriptWitness = ScriptWitnessImpl(asm)
}