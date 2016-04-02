package org.scalacoin.script.constant

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/6/16.
 */
class ConstantsTest extends FlatSpec with MustMatchers {

  "Constants" must "define an OP_FALSE" in {
    OP_FALSE.opCode must be (0)
    OP_FALSE.hex must be ("00")
    OP_FALSE.scriptNumber must be (OP_0.scriptNumber)
    OP_FALSE.bytes.isEmpty must be (true)
    OP_FALSE.bytesSize must be (1)
  }

  it must "define an OP_PUSHDATA1" in {
    OP_PUSHDATA1.opCode must be (76)
    OP_PUSHDATA1.hex must be ("4c")
  }

  it must "define an OP_PUSHDATA2" in {
    OP_PUSHDATA2.opCode must be (77)
    OP_PUSHDATA2.hex must be ("4d")
  }

  it must "define an OP_PUSHDATA4" in {
    OP_PUSHDATA4.opCode must be (78)
    OP_PUSHDATA4.hex must be ("4e")
  }

  it must "define an OP_1NEGATE" in {
    OP_1NEGATE.opCode must be (79)
    OP_1NEGATE.hex must be ("4f")
    OP_1NEGATE.scriptNumber must be (ScriptNumberImpl(-1))
  }

  it must "define an OP_TRUE" in {
    OP_TRUE.opCode must be (81)
    OP_TRUE.hex must be ("51")
    OP_TRUE.scriptNumber must be (ScriptNumberImpl(1))
  }

  it must "define an OP_2" in {
    OP_2.opCode must be (82)
    OP_2.hex must be ("52")
    OP_2.scriptNumber must be (ScriptNumberImpl(2))
  }
  it must "define an OP_3" in {
    OP_3.opCode must be (83)
    OP_3.hex must be ("53")
    OP_3.scriptNumber must be (ScriptNumberImpl(3))
  }
  it must "define an OP_4" in {
    OP_4.opCode must be (84)
    OP_4.hex must be ("54")
    OP_4.scriptNumber must be (ScriptNumberImpl(4))
  }
  it must "define an OP_5" in {
    OP_5.opCode must be (85)
    OP_5.hex must be ("55")
    OP_5.scriptNumber must be (ScriptNumberImpl(5))
  }
  it must "define an OP_6" in {
    OP_6.opCode must be (86)
    OP_6.hex must be ("56")
    OP_6.scriptNumber must be (ScriptNumberImpl(6))
  }
  it must "define an OP_7" in {
    OP_7.opCode must be (87)
    OP_7.hex must be ("57")
    OP_7.scriptNumber must be (ScriptNumberImpl(7))

  }
  it must "define an OP_8" in {
    OP_8.opCode must be (88)
    OP_8.hex must be ("58")
    OP_8.scriptNumber must be (ScriptNumberImpl(8))
  }
  it must "define an OP_9" in {
    OP_9.opCode must be (89)
    OP_9.hex must be ("59")
    OP_9.scriptNumber must be (ScriptNumberImpl(9))
  }
  it must "define an OP_10" in {
    OP_10.opCode must be (90)
    OP_10.hex must be ("5a")
    OP_10.scriptNumber must be (ScriptNumberImpl(10))
  }
  it must "define an OP_11" in {
    OP_11.opCode must be (91)
    OP_11.hex must be ("5b")
    OP_11.scriptNumber must be (ScriptNumberImpl(11))
  }
  it must "define an OP_12" in {
    OP_12.opCode must be (92)
    OP_12.hex must be ("5c")
    OP_12.scriptNumber must be (ScriptNumberImpl(12))
  }
  it must "define an OP_13" in {
    OP_13.opCode must be (93)
    OP_13.hex must be ("5d")
    OP_13.scriptNumber must be (ScriptNumberImpl(13))
  }
  it must "define an OP_14" in {
    OP_14.opCode must be (94)
    OP_14.hex must be ("5e")
    OP_14.scriptNumber must be (ScriptNumberImpl(14))
  }
  it must "define an OP_15" in {
    OP_15.opCode must be (95)
    OP_15.hex must be ("5f")
    OP_15.scriptNumber must be (ScriptNumberImpl(15))
  }
  it must "define an OP_16" in {
    OP_16.opCode must be (96)
    OP_16.hex must be ("60")
    OP_16.scriptNumber must be (ScriptNumberImpl(16))
  }

  it must "produce the correct hex for a negative number" in {
    val number = ScriptNumberImpl(-1)
    val expectedHex = "81"
    number.hex must be (expectedHex)

    val number1 = ScriptNumberImpl(-127)
    val expectedHex1 = "ff"
    number1.hex must be (expectedHex1)

    val number2 = ScriptNumberImpl(-128)
    val expectedHex2 = "8080"
    number2.hex must be (expectedHex2)

    val number3 = ScriptNumberImpl(-32768)
    val expectedHex3 = "008080"
    number3.hex must be (expectedHex3)
  }

  it must "evaluate ScriptTrue to non zero" in {
    ScriptTrue.hex must not equal ("00")
  }





}
