package org.bitcoins.core.script.interpreter.testprotocol

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptSignature,
  ScriptWitness
}
import org.bitcoins.core.script.result.ScriptResult

/** Created by chris on 1/18/16.
  * This represents a core test case for valid and invalid scripts
  * the scripts can be seen in the script_tests.json file
  * files.
  */
trait CoreTestCase {
  def scriptSig: ScriptSignature
  def scriptPubKey: ScriptPubKey
  def flags: String
  def expectedResult: ScriptResult
  def comments: String
  def raw: String
  def witness: Option[(ScriptWitness, CurrencyUnit)]
}

case class CoreTestCaseImpl(
    scriptSig: ScriptSignature,
    scriptPubKey: ScriptPubKey,
    flags: String,
    expectedResult: ScriptResult,
    comments: String,
    raw: String,
    witness: Option[(ScriptWitness, CurrencyUnit)])
    extends CoreTestCase
