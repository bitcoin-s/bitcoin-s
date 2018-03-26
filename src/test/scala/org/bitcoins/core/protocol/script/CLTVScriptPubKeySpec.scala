package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.{ TransactionGenerators, ScriptGenerators }
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{ Properties, Prop }

/**
 * Created by tom on 8/23/16.
 */
class CLTVScriptPubKeySpec extends Properties("CLTVScriptPubKeySpec") {
  property("Serialization symmetry") =
    Prop.forAll(ScriptGenerators.cltvScriptPubKey) {
      case (cltvScriptPubKey, _) =>
        CLTVScriptPubKey(cltvScriptPubKey.hex) == cltvScriptPubKey
    }
}
