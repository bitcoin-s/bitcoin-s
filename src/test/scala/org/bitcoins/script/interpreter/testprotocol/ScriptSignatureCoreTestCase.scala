package org.bitcoins.script.interpreter.testprotocol

import org.bitcoins.protocol.script.{ScriptSignature, ScriptPubKey}
import org.bitcoins.script.constant.ScriptToken

/**
 * Created by chris on 3/14/16.
 */
trait ScriptSignatureCoreTestCase {
  /**
   * The parsed asm representation for the core test case
   * this will be different than the asm representation
   * inside of scriptSignature
 *
   * @return
   */
  def asm : Seq[ScriptToken]

  /**
   * This is the underlying scriptSignature that is parsed from the core test case
   * this is needed because there is no ubiquitous formats for scriptSignatures
   * inside of script_valid.json. Normal scriptSignatures have their asm representation
   * parsed from the underlying hex/byte representation every time which won't work
   * for core test cases.
 *
   * @return
   */
  def scriptSignature : ScriptSignature

}

case class ScriptSignatureCoreTestCaseImpl(asm : Seq[ScriptToken], scriptSignature : ScriptSignature) extends ScriptSignatureCoreTestCase
