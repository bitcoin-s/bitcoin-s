package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{ Prop, Properties }

/**
 * Created by chris on 6/22/16.
 */
class P2PKScriptSignatureSpec extends Properties("P2PKSpec") {
  private def logger = BitcoinSLogger.logger

  property("Serialization symmetry") =
    Prop.forAll(ScriptGenerators.p2pkScriptSignature) { p2pkScriptSig =>
      logger.info("P2PKScriptSig: " + p2pkScriptSig)
      logger.info("p2pk hex: " + p2pkScriptSig.hex)
      P2PKScriptSignature(p2pkScriptSig.hex) == p2pkScriptSig
    }
}
