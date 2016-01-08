package org.scalacoin.script.parsing

import org.scalacoin.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/7/16.
 */
class ScriptParserTest extends FlatSpec with MustMatchers with ScriptParser {


  "ScriptParser" must "parse an input script" in {
    val parsedInput = parseInputScript(TestUtil.p2pkhInputScriptNotParsed)
    parsedInput must be (TestUtil.p2pkhInputScript)
  }

  it must "parse an output script" in {
    val parsedOutput = parseOutputScript(TestUtil.p2pkhOutputScriptNotParsed)
    parsedOutput must be (TestUtil.p2pkhOutputScript)
  }
}
