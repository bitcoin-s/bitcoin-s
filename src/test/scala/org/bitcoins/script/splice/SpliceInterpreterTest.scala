package org.bitcoins.script.splice

import org.bitcoins.script.{ScriptProgram}
import org.bitcoins.script.bitwise.OP_EQUAL
import org.bitcoins.script.constant._
import org.bitcoins.util.TestUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 2/4/16.
 */
class SpliceInterpreterTest extends FlatSpec with MustMatchers with SpliceInterpreter {

  "SpliceInterpreter" must "evaluate an OP_SIZE on OP_0 correctly" in {
    val stack = List(OP_0)
    val script = List(OP_SIZE)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opSize(program)
    newProgram.stack must be (List(OP_0,OP_0))
    newProgram.script.isEmpty must be (true)

  }

  it must "deterine the size of script number 0 correctly" in {
    val stack = List(ScriptNumber.zero)
    val script = List(OP_SIZE)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opSize(program)
    newProgram.stack must be (List(ScriptNumber.zero,ScriptNumber.zero))
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_SIZE correctly with 0x7f" in {
    val stack = List(ScriptConstant("7f"))
    val script = List(OP_SIZE)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opSize(program)
    newProgram.stack must be (List(ScriptNumberImpl(1),ScriptConstantImpl("7f")))
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_SIZE correctly with 0x8000" in {
    //0x8000 == 128 in bitcoin
    val stack = List(ScriptNumber(128))
    val script = List(OP_SIZE)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opSize(program)
    newProgram.stack must be (List(ScriptNumber(2), ScriptNumber(128)))
    newProgram.script.isEmpty must be (true)
  }


  it must "evaluate an OP_SIZE correctly with a negative number" in {
    val stack = List(ScriptNumber(-1))
    val script = List(OP_SIZE)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opSize(program)
    newProgram.stack must be (List(ScriptNumber.one,ScriptNumber(-1)))
    newProgram.script.isEmpty must be (true)
  }
}
