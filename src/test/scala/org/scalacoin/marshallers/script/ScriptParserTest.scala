package org.scalacoin.marshallers.script

import org.scalacoin.util.{ScalacoinUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/7/16.
 */
class ScriptParserTest extends FlatSpec with MustMatchers with ScriptParser with ScalacoinUtil {


  "ScriptParser" must "parse an input script" in {
    val parsedInput = parse(TestUtil.p2pkhInputScriptNotParsedAsm)
    parsedInput must be (TestUtil.p2pkhInputScriptAsm)
  }

  it must "parse a pay-to-pubkey-hash output script" in {
    val parsedOutput = parse(TestUtil.p2pkhOutputScriptNotParsedAsm)
    parsedOutput must be (TestUtil.p2pkhOutputScriptAsm)
  }

  it must "parse a pay-to-script-hash output script" in {
    val parsedOutput = parse(TestUtil.p2shOutputScriptNotParsedAsm)
    parsedOutput must be (TestUtil.p2shOutputScriptAsm)
  }

  it must "parse a pay-to-script-hash input script" in {
    val parsedInput = parse(TestUtil.p2shInputScriptNotParsedAsm)
    parsedInput must be (TestUtil.p2shInputScriptAsm)
  }

  it must "parse a p2pkh output script from a byte array to script tokens" in {
    val bytes : List[Byte] = decodeHex(TestUtil.p2pkhOutputScript)
    parse(bytes) must be (TestUtil.p2pkhOutputScriptAsm)
  }

  it must "parse a p2pkh input script from a byte array to script tokens" in {
    val bytes = decodeHex(TestUtil.p2pkhInputScript)
    parse(bytes) must be (TestUtil.p2pkhInputScriptAsm)
  }

  it must "parse a p2sh input script from a byte array into script tokens" in {
    val bytes = decodeHex(TestUtil.p2shInputScript)
    parse(bytes) must be (TestUtil.p2shInputScriptAsm)
  }

  it must "parse a p2sh outputscript from a byte array into script tokens" in {
    val bytes = decodeHex(TestUtil.p2shOutputScript)
    parse(bytes) must be (TestUtil.p2shOutputScriptAsm)
  }

}
