package org.scalacoin.marshallers.script

import org.scalacoin.script.arithmetic.OP_ADD
import org.scalacoin.script.bitwise.OP_EQUAL
import org.scalacoin.script.constant._
import org.scalacoin.script.control.{OP_ENDIF, OP_IF}
import org.scalacoin.script.reserved.OP_NOP
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
/*  it must "parse a number larger than an integer into a ScriptNumberImpl" in {
    parse("2147483648") must be (List(ScriptNumberImpl(2147483648L)))
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

  it must "parse a script number that has a leading zero" in {
    val str = "0x0100"
    parse(str) must equal (List(ScriptNumberImpl(1)))
  }


  it must "parse an OP_PICK" in {
    val str = "PICK"
    parse(str) must equal (List(OP_PICK))
  }

  it must "parse an OP_NOP" in {
    val str = "NOP"
    parse(str) must equal (List(OP_NOP))
  }

  it must "parse a script that has a decimal and a hexadecimal number in it " in  {
    val str = "32767 0x02 0xff7f EQUAL"
    parse(str) must equal (List(ScriptNumberImpl(32767), BytesToPushOntoStackImpl(2), ScriptNumberImpl(32767), OP_EQUAL))
  }

  it must "parse an OP_1" in {
    val str = "0x51"
    parse(str) must equal (List(OP_1))
  }

  it must "parse an extremely long hex constant" in {
    val str = "0x4b 0x417a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a " +
    "'Azzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz' EQUAL"

    parse(str) must equal (List(BytesToPushOntoStackImpl(75),
      ScriptConstantImpl("417a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a"),
      ScriptConstantImpl("417a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a"), OP_EQUAL))
  }

  it must "parse a hexadecimal number and a string constant as the same thing" in {
    val str = "0x02 0x417a 'Az' EQUAL"
    parse(str) must be (List(BytesToPushOntoStackImpl(2),ScriptNumberImpl(31297),ScriptConstantImpl("417a"),OP_EQUAL))
  }

  it must "parse a combination of decimal and hexadecimal constants correctly" in {
    val str = "127 0x01 0x7F EQUAL"
    parse(str) must be (List(ScriptNumberImpl(127), BytesToPushOntoStackImpl(1), ScriptNumberImpl(127), OP_EQUAL))
  }*/

  it must "parse an OP_IF OP_ENDIF block" in {
    val str = "1 0x01 0x80 IF 0 ENDIF"
    parse(str) must be (List(OP_1, BytesToPushOntoStackImpl(1), ScriptNumberImpl(0), OP_IF, OP_0, OP_ENDIF))
  }

}
