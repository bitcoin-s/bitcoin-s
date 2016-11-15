package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.script.result.{ScriptError, ScriptErrorDiscourageUpgradeableWitnessProgram, ScriptErrorWitnessProgramMisMatch, ScriptErrorWitnessProgramWrongLength}

/**
  * Created by chris on 11/10/16.
  */
sealed trait WitnessVersion {
  /** Rebuilds the full script from the given witness and [[ScriptPubKey]]
    * Either returns the stack and the [[ScriptPubKey]] it needs to be executed against or
    * the [[ScriptError]] that was encountered while rebuilding the witness*/
  def rebuild(scriptWitness: ScriptWitness): Either[(Seq[ScriptToken], ScriptPubKey),ScriptError]
}

case object WitnessVersion0 extends WitnessVersion {

  override def rebuild(scriptWitness: ScriptWitness): Either[(Seq[ScriptToken], ScriptPubKey),ScriptError] = {
    val scriptBytes = scriptWitness.stack.map(_.bytes).flatten

    scriptBytes.size match {
      case 20 =>
        //p2wpkh
        if (scriptWitness.stack.size != 2) Right(ScriptErrorWitnessProgramMisMatch)
        else {
          val key = ECPublicKey(scriptWitness.stack.last.bytes)
          Left((scriptWitness.stack, P2PKHScriptPubKey(key)))
        }
      case 32 =>
        //p2wsh
        if (scriptWitness.stack.size == 0) Right(ScriptErrorWitnessProgramWrongLength)
        else {
          val scriptPubKey = ScriptPubKey(scriptWitness.stack.last.bytes)
          val stack = scriptWitness.stack.slice(0,scriptWitness.stack.size - 1)
          Left(stack,scriptPubKey)
        }
      case _ =>
        //witness version 0 programs need to be 20 bytes or 32 bytes in size
        Right(ScriptErrorWitnessProgramWrongLength)
    }
  }
}

/** The witness version that represents all witnesses that have not been allocated yet */

case object UnassignedWitness extends WitnessVersion {
  override def rebuild(scriptWitness: ScriptWitness): Either[(Seq[ScriptToken], ScriptPubKey),ScriptError] =
    Right(ScriptErrorDiscourageUpgradeableWitnessProgram)
}

object WitnessVersion {
  def apply(num: Long): WitnessVersion = num match {
    case 0 => WitnessVersion0
    case _ => UnassignedWitness
  }
}
