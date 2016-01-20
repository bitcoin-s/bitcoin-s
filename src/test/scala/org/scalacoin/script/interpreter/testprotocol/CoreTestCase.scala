package org.scalacoin.script.interpreter.testprotocol

import org.scalacoin.protocol.script.{ScriptPubKey, ScriptSignature}

/**
 * Created by chris on 1/18/16.
 * This represents a core test case for valid and invalid scripts
 * the scripts can be seen in the ../script_valid.json and ../script_invalid.json
 * files.
 */
trait CoreTestCase {
  def scriptSig : ScriptSignature
  def scriptPubKey : ScriptPubKey
  def flags : String
  def comments : String
  def raw : String
}

case class CoreTestCaseImpl(scriptSig : ScriptSignature,
  scriptPubKey: ScriptPubKey, flags : String, comments : String, raw : String) extends CoreTestCase
