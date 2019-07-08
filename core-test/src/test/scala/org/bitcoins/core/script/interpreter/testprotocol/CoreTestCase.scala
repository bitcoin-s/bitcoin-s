package org.bitcoins.core.script.interpreter.testprotocol

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptSignature,
  ScriptWitness
}
import org.bitcoins.core.script.result.ScriptResult

/**
  * This represents a test case for valid and invalid scripts.
  * The scripts can be seen in the `script_tests.json` file
  * found in the resource directory..
  */
case class CoreTestCase(
    scriptSig: ScriptSignature,
    scriptPubKey: ScriptPubKey,
    flags: String,
    expectedResult: ScriptResult,
    comments: String,
    raw: String,
    witness: Option[(ScriptWitness, CurrencyUnit)])
