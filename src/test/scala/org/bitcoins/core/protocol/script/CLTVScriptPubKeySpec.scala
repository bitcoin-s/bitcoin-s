package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.{TransactionGenerators, ScriptGenerators}
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Properties, Prop}

/**
  * Created by tom on 8/23/16.
  */
class CLTVScriptPubKeySpec extends Properties("CLTVScriptPubKeySpec") with BitcoinSLogger {
  property("Serialization symmetry") =
    Prop.forAll(ScriptGenerators.cltvScriptPubKey) { cltvScriptPubKey =>
      CLTVScriptPubKey(cltvScriptPubKey.hex) == cltvScriptPubKey
    }
  property("a valid unspendable CLTV Transaction's locktime must be less than the script's CLTV value ") =
    Prop.forAll(TransactionGenerators.unspendableCLTVTransaction) { txSigComponent =>
      val locktime = txSigComponent._1.transaction.lockTime.underlying
      val cltvScriptValue = txSigComponent._3.underlying
      locktime < cltvScriptValue
    }

  property("a valid spendable CLTV Transaction's locktime must be greater than the script's CLTV value ") =
    Prop.forAll(TransactionGenerators.spendableCLTVTransaction) { txSigComponent =>
      val locktime = txSigComponent._1.transaction.lockTime.underlying
      val cltvScriptValue = txSigComponent._3.underlying
      locktime > cltvScriptValue
    }
}
