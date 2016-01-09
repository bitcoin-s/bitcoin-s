package org.scalacoin.script

import org.scalacoin.script.arithmetic.OP_1ADD
import org.scalacoin.script.bitwise.OP_EQUAL
import org.scalacoin.script.constant.{OP_1, OP_TRUE, OP_0}
import org.scalacoin.script.control.OP_IF
import org.scalacoin.script.crypto.OP_RIPEMD160
import org.scalacoin.script.locktime.OP_CHECKLOCKTIMEVERIFY
import org.scalacoin.script.stack.OP_TOALTSTACK
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/9/16.
 */
class ScriptOperationFactoryTest extends FlatSpec with MustMatchers {

  "ScriptOperationFactory" must "match operations with their byte representation" in {
    ScriptOperationFactory.fromOpCode(0x00) must be (Some(OP_0))
    ScriptOperationFactory.fromOpCode(0x51) must be (Some(OP_1))

    ScriptOperationFactory.fromOpCode(0x63) must be (Some(OP_IF))
    ScriptOperationFactory.fromOpCode(0x6b) must be (Some(OP_TOALTSTACK))

    ScriptOperationFactory.fromOpCode(135.toByte) must be (Some(OP_EQUAL))

    ScriptOperationFactory.fromOpCode(139.toByte) must be (Some(OP_1ADD))

    ScriptOperationFactory.fromOpCode(166.toByte) must be (Some(OP_RIPEMD160))
    ScriptOperationFactory.fromOpCode(177.toByte) must be (Some(OP_CHECKLOCKTIMEVERIFY))

  }

}
