package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.scalacheck.{ Prop, Properties }

/**
 * Created by tom on 8/23/16.
 */
class CSVScriptPubKeySpec extends Properties("CSVScriptPubKeySpec") {
  property("Serialization Symmetry") =
    Prop.forAll(ScriptGenerators.csvScriptPubKey) {
      case (csvScriptPubKey, _) =>
        CSVScriptPubKey(csvScriptPubKey.hex) == csvScriptPubKey
    }
}
