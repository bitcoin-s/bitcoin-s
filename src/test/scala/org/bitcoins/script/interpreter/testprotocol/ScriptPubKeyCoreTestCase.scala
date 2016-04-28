package org.bitcoins.script.interpreter.testprotocol

import org.bitcoins.protocol.script.ScriptPubKey
import org.bitcoins.script.constant.ScriptToken

/**
 * Created by chris on 3/14/16.
 */
trait ScriptPubKeyCoreTestCase {
  /**
   * The parsed asm representation for the core test case
   * this will be different than the asm representation
   * inside of scriptPubKey
 *
   * @return
   */
  def asm : Seq[ScriptToken]

  /**
   * This is the underlying scriptPubKey that is parsed from the core test case
   * this is needed because there is no ubiquitous formats for scriptPubKeys
   * inside of script_valid.json. Normal scriptPubKeys have their asm representation
   * parsed from the underlying hex/byte representation every time which won't work
   * for core test cases.
 *
   * @return
   */
  def scriptPubKey : ScriptPubKey
}


case class ScriptPubKeyCoreTestCaseImpl(asm : Seq[ScriptToken], scriptPubKey : ScriptPubKey) extends ScriptPubKeyCoreTestCase