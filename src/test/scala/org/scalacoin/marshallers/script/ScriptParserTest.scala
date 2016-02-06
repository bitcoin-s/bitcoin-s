package org.scalacoin.marshallers.script

import org.scalacoin.script.arithmetic.OP_ADD
import org.scalacoin.script.bitwise.OP_EQUAL
import org.scalacoin.script.constant._
import org.scalacoin.script.stack.OP_PICK
import org.scalacoin.util.{ScalacoinUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/7/16.
 */
class ScriptParserTest extends FlatSpec with MustMatchers with ScriptParser with ScalacoinUtil {



  "ScriptParser" must "parse 0x00 to a OP_0" in {
    parse(List(0.toByte)) must be (List(OP_0))
  }
  it must "parse a pay-to-pubkey-hash output script" in {
    val parsedOutput = parse(TestUtil.p2pkhOutputScriptNotParsedAsm)
    parsedOutput must be (TestUtil.p2pkhOutputScriptAsm)
  }

  it must "parse a pay-to-script-hash output script" in {
    val parsedOutput = parse(TestUtil.p2shOutputScriptNotParsedAsm)
    parsedOutput must be (TestUtil.p2shOutputScriptAsm)
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

  it must "parse a script constant from 'Az' EQUAL" in {
    val str = "'Az' EQUAL"
    parse(str) must equal (List(ScriptConstantImpl("417a"), OP_EQUAL))
  }

  it must "parse a script constant that has a leading zero" in {
    val str = "0x0100"
    parse(str) must equal (List(ScriptConstantImpl("0100")))
  }

  it must "parse a script signature with a decimal constant in it" in {
    val str = "0x4f 1000 ADD"
    //0x3e8 == 1000
    parse(str) must equal (List(OP_1NEGATE, ScriptConstantImpl("3e8"), OP_ADD))
  }

  it must "parse an OP_PICK" in {
    val str = "PICK"
    parse(str) must equal (List(OP_PICK))
  }





}
