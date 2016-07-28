package org.bitcoins.core.crypto

import org.bitcoins.core.gen.TransactionGenerators
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.script.{PreExecutionScriptProgram, ScriptProgram}
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 7/25/16.
  */
class TransactionSignatureCreatorSpec extends Properties("TransactionSignatureCreatorSpec") with BitcoinSLogger {

  property("Must generate a valid signature for a p2pk transaction") =
    Prop.forAll(TransactionGenerators.signedP2PKTransaction) {
      case (txSignatureComponent: TransactionSignatureComponent, _) =>
        //run it through the interpreter
        val program: PreExecutionScriptProgram = ScriptProgram(txSignatureComponent)
        val result = ScriptInterpreter.run(program)
        result == ScriptOk
    }

  property("generate a valid signature for a p2pkh transaction") =
    Prop.forAll(TransactionGenerators.signedP2PKHTransaction) {
      case (txSignatureComponent: TransactionSignatureComponent, _) =>
        //run it through the interpreter
        val program = ScriptProgram(txSignatureComponent)
        val result = ScriptInterpreter.run(program)
        result == ScriptOk
    }

  property("generate valid signatures for a multisignature transaction") =
    Prop.forAllNoShrink(TransactionGenerators.signedMultiSigTransaction) {
      case (txSignatureComponent: TransactionSignatureComponent, _)  =>
        //run it through the interpreter
        val program = ScriptProgram(txSignatureComponent)

        val result = ScriptInterpreter.run(program)
        result == ScriptOk
  }

  property("generate a valid signature for a p2sh transaction") =
    Prop.forAll(TransactionGenerators.p2SHTransaction) {
      case (txSignatureComponent: TransactionSignatureComponent, _) =>
        //run it through the interpreter
        val program = ScriptProgram(txSignatureComponent)
        val result = ScriptInterpreter.run(program)
        result == ScriptOk
    }
}
