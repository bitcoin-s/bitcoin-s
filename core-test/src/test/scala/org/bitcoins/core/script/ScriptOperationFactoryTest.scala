package org.bitcoins.core.script

import org.bitcoins.core.script.arithmetic.OP_1ADD
import org.bitcoins.core.script.bitwise.OP_EQUAL
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control.OP_IF
import org.bitcoins.core.script.crypto.{OP_CHECKSIGADD, OP_RIPEMD160}
import org.bitcoins.core.script.locktime.OP_CHECKLOCKTIMEVERIFY
import org.bitcoins.core.script.splice.OP_SUBSTR
import org.bitcoins.core.script.stack.OP_TOALTSTACK
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 1/9/16.
  */
class ScriptOperationFactoryTest extends BitcoinSUnitTest {

  "ScriptOperationFactory" must "match operations with their byte representation" in {

    ScriptOperation(0x00.toByte) must be(OP_0)
    ScriptOperation(0x51.toByte) must be(OP_1)

    ScriptOperation(0x63.toByte) must be(OP_IF)
    ScriptOperation(0x6b.toByte) must be(OP_TOALTSTACK)

    ScriptOperation(135.toByte) must be(OP_EQUAL)

    ScriptOperation(139.toByte) must be(OP_1ADD)

    ScriptOperation(166.toByte) must be(OP_RIPEMD160)
    ScriptOperation(177.toByte) must be(OP_CHECKLOCKTIMEVERIFY)

    ScriptOperation.fromByte(0xba.toByte) must be(OP_CHECKSIGADD)

  }

  it must "find OP_4 from it's byte representation" in {
    val byteRepresentation = BytesUtil.decodeHex("54").head
    ScriptOperation(byteRepresentation) must be(OP_4)
  }

  it must "find a byte to push onto stack from its byte representation" in {
    val result = ScriptOperation(2.toByte)
    result must be(BytesToPushOntoStack(2))
  }

  it must "find a script number from its hex representation" in {
    val result = ScriptOperation("02")
    result.isDefined must be(true)
    result.get must be(BytesToPushOntoStack(2))
  }

  it must "find undefined op codes" in {
    val result = ScriptOperation("ba")
    result.isDefined must be(true)
  }

  it must "find a splice operation from it's hex representation" in {
    val spliceOperation = ScriptOperation("7F")
    spliceOperation.isDefined must be(true)
    spliceOperation.get must be(OP_SUBSTR)
  }

  it must "find OP_1NEGATE from its hex representation" in {
    OP_1NEGATE.opCode must be(79)
    OP_1NEGATE.toByte must be(79)
    val negateOperation = ScriptOperation("4f")
    negateOperation.isDefined must be(true)
    negateOperation.get must be(OP_1NEGATE)
  }

}
