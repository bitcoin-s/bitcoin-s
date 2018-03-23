package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{ Prop, Properties }

/**
 * Created by chris on 6/24/16.
 */
class P2SHScriptSignatureSpec extends Properties("P2SHScriptSignatureSpec") {

  property("Symmetrical serialization") =
    Prop.forAll(ScriptGenerators.p2shScriptSignature) { p2shScriptSig =>
      P2SHScriptSignature(p2shScriptSig.hex) == p2shScriptSig

    }

  property("place a witness scriptPubKey in a p2shScriptSig, then extract the witScriptPubKey again") =
    Prop.forAll(ScriptGenerators.witnessScriptPubKeyV0) {
      case (witScriptPubKey, privKeys) =>
        val p2shScriptSig = P2SHScriptSignature(witScriptPubKey)
        p2shScriptSig.redeemScript == witScriptPubKey
        p2shScriptSig.scriptSignatureNoRedeemScript.get == EmptyScriptSignature

    }
}
