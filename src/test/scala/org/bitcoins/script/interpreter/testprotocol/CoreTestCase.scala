package org.bitcoins.script.interpreter.testprotocol

import org.bitcoins.protocol.script.{ScriptPubKey, ScriptSignature}

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
  def comments : String
  def raw : String
}

case class CoreTestCaseImpl(scriptSig : ScriptSignatureCoreTestCase,
  scriptPubKey: ScriptPubKeyCoreTestCase, flags : String, comments : String, raw : String) extends CoreTestCase
