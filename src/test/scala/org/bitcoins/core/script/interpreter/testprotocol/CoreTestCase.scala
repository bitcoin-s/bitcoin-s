package org.bitcoins.core.script.interpreter.testprotocol

import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.script.result.ScriptResult

/**
 * Created by chris on 1/18/16.
 * This represents a core test case for valid and invalid scripts
 * the scripts can be seen in the ../script_valid.json and ../script_invalid.json
 * files.
 */
trait CoreTestCase {
  def scriptSig : ScriptSignatureCoreTestCase
  def scriptPubKey : ScriptPubKeyCoreTestCase
  def flags : String
  def expectedResult : ScriptResult
  def comments : String
  def raw : String
}

case class CoreTestCaseImpl(scriptSig : ScriptSignatureCoreTestCase,
  scriptPubKey: ScriptPubKeyCoreTestCase, flags : String, expectedResult : ScriptResult,
  comments : String, raw : String) extends CoreTestCase
