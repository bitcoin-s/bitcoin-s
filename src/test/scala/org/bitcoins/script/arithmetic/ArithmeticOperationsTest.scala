package org.bitcoins.script.arithmetic

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/6/16.
 */
class ArithmeticOperationsTest extends FlatSpec with MustMatchers {

  "ArithmeticOperatoins" must "define OP_1ADD" in {
    OP_1ADD.opCode must be (139)
  }

  it must "define OP_1SUB" in {
    OP_1SUB.opCode must be (140)
  }

  it must "define OP_NEGATE" in {
    OP_NEGATE.opCode must be (143)
  }

  it must "define OP_ABS" in {
    OP_ABS.opCode must be (144)
  }

  it must "define OP_NOT" in {
    OP_NOT.opCode must be (145)
  }
  it must "define OP_0NOTEQUAL" in {
    OP_0NOTEQUAL.opCode must be (146)
    OP_0NOTEQUAL.hex must be ("92")
  }

  it must "define OP_ADD" in {
    OP_ADD.opCode must be (147)
  }

  it must "define OP_SUB" in {
    OP_SUB.opCode must be (148)
  }

  it must "define OP_BOOLAND" in {
    OP_BOOLAND.opCode must be (154)
  }

  it must "define OP_BOOLOR" in {
    OP_BOOLOR.opCode must be (155)
  }

  it must "define OP_NUMEQUAL" in {
    OP_NUMEQUAL.opCode must be (156)
  }

  it must "define OP_NUMEQUALVERIFY" in {
    OP_NUMEQUALVERIFY.opCode must be (157)
  }

  it must "define OP_NUMNOTEQUAL" in {
    OP_NUMNOTEQUAL.opCode must be (158 )
  }

  it must "define OP_LESSTHAN" in {
    OP_LESSTHAN.opCode must be (159)
  }

  it must "define OP_GREATERTHAN" in {
    OP_GREATERTHAN.opCode must be (160)
  }

  it must "define OP_LESSTHANOREQUAL" in {
    OP_LESSTHANOREQUAL.opCode must be (161)
  }

  it must "define OP_GREATERTHANOREQUAL" in {
    OP_GREATERTHANOREQUAL.opCode must be (162)
  }

  it must "define OP_MIN" in {
    OP_MIN.opCode must be (163)
  }

  it must "define OP_MAX" in {
    OP_MAX.opCode must be (164)
  }

  it must "define OP_WITHIN" in {
    OP_WITHIN.opCode must be (165)
  }
}
