package org.scalacoin.script.parsing

import org.scalacoin.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/7/16.
 */
class ScriptParserTest extends FlatSpec with MustMatchers with ScriptParser {


  "ScriptParser" must "parse an input script" in {
/*    val parsedInput = parseInputScript(TestUtil.p2pkhInputScriptNotParsedAsm)
    parsedInput must be (TestUtil.p2pkhInputScriptAsm)*/
  }

  it must "parse a pay-to-pubkey-hash output script" in {
    val parsedOutput = parseOutputScript(TestUtil.p2pkhOutputScriptNotParsedAsm)
    parsedOutput must be (TestUtil.p2pkhOutputScriptAsm)
  }

  it must "parse a pay-to-script-hash output script" in {
    val parsedOutput = parseOutputScript(TestUtil.p2shOutputScriptNotParsedAsm)
    parsedOutput must be (TestUtil.p2shOutputScriptAsm)
  }

/*  it must "parse a pay-to-script-hash input script" in {
    val parsedInput = parseInput
  }*/

}
