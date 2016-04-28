package org.bitcoins.script.stack

import org.bitcoins.script.error.ScriptErrorInvalidStackOperation
import org.bitcoins.script.{ScriptProgram}
import org.bitcoins.script.bitwise.OP_EQUAL
import org.bitcoins.script.constant._
import org.bitcoins.util.{ScriptProgramTestUtil, BitcoinSUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/6/16.
 */
class StackInterpreterTest extends FlatSpec with MustMatchers with StackInterpreter {
  val stack = List(ScriptConstantImpl("Hello"),ScriptConstantImpl("World"))
  "StackInterpreter" must "duplicate elements on top of the stack" in {

    val script = List(OP_DUP)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opDup(program)

    newProgram.stack.head must be (ScriptConstantImpl("Hello"))
    newProgram.stack(1) must be (ScriptConstantImpl("Hello"))
    newProgram.stack(2) must be (ScriptConstantImpl("World"))
  }

  it must "throw an exception when calling opDup without an OP_DUP on top of the script stack" in {

    intercept[IllegalArgumentException] {
      val script = List()
      val program = ScriptProgram(TestUtil.testProgram, stack,script)
      opDup(program)
    }
  }

  it must "mark the script invalid when calling opDup without an element on top of the stack" in {
    val stack = List()
    val script = List(OP_DUP)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    ScriptProgramTestUtil.toExecutedScriptProgram(opDup(program)).error must be (Some(ScriptErrorInvalidStackOperation))

  }

  it must "evaluate the OP_DEPTH operator correctly" in {
    val stack = List(ScriptConstantImpl("Hello"),ScriptConstantImpl("World"))
    val script = List(OP_DEPTH)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opDepth(program)

    newProgram.stack.head.hex must be (BitcoinSUtil.encodeHex(stack.size.toByte))
  }

  it must "evaluate OP_DEPTH operator correctly when there are zero items on the stack" in {
    val stack = List()
    val script = List(OP_DEPTH)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opDepth(program)
    newProgram.stack.head must be (ScriptNumber.zero)
  }

  it must "evaluate an OP_TOALTSTACK operator correctly" in {
    val stack = List(OP_0)
    val script = List(OP_TOALTSTACK)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opToAltStack(program)

    newProgram.stack.isEmpty must be (true)
    newProgram.script.isEmpty must be (true)
    newProgram.altStack must be (List(OP_0))

  }

  it must "evaluate an OP_DROP operator correctly" in {
    val stack = List(OP_0)
    val script = List(OP_DROP)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opDrop(program)

    newProgram.stack.isEmpty must be (true)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_IFDUP correctly" in {
    val stack = List(ScriptNumber.zero)
    val script = List(OP_IFDUP)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opIfDup(program)

    newProgram.stack must be (stack)
    newProgram.script.isEmpty must be (true)

    val stack1 = List(OP_1)
    val program1 = ScriptProgram(TestUtil.testProgram, stack1,script)
    val newProgram1 = opIfDup(program1)
    newProgram1.stack must be (List(OP_1,OP_1))
    newProgram1.script.isEmpty must be (true)

  }

  it must "evaluate an OP_NIP correctly" in {
    val stack = List(OP_0,OP_1)
    val script = List(OP_NIP)

    val program = ScriptProgram(TestUtil.testProgram, stack,script)

    val newProgram = opNip(program)

    newProgram.stack must be (List(OP_0))
    newProgram.script.isEmpty must be (true)
  }

  it must "mark the script as invalid if there is less than 2 elements on the stack for OP_NIP" in {
    val stack = List(OP_0)
    val script = List(OP_NIP)

    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(opNip(program))
    newProgram.error must be (Some(ScriptErrorInvalidStackOperation))

  }

  it must "throw an exception if there is no elements on the stack for OP_NIP" in {
    val stack = List()
    val script = List(OP_NIP)

    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(opNip(program))
    newProgram.error must be (Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_OVER correctly" in {
    val stack = List(OP_0,OP_1)
    val script = List(OP_OVER)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opOver(program)
    newProgram.stack must be (List(OP_1,OP_0,OP_1))
    newProgram.script.isEmpty must be (true)
  }

  it must "mark the script as invalid if there is less than 2 elements on the stack for OP_OVER" in {
    val stack = List(OP_0)
    val script = List(OP_OVER)

    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(opOver(program))
    newProgram.error must be (Some(ScriptErrorInvalidStackOperation))

  }

  it must "mark the script as invalid if there is no elements on the stack for OP_OVER" in {
    val stack = List()
    val script = List(OP_OVER)

    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(opOver(program))
    newProgram.error must be (Some(ScriptErrorInvalidStackOperation))

  }

  it must "evaluate an OP_PICK correctly" in {
    val stack = List(ScriptNumber.zero, ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"))
    val script = List(OP_PICK)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opPick(program)

    newProgram.stack must be (List(ScriptConstantImpl("14"),ScriptConstantImpl("14"),
      ScriptConstantImpl("15"), ScriptConstantImpl("16")))
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_ROLL correctly" in {
    val stack = List(ScriptNumber.zero, ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"))
    val script = List(OP_ROLL)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val newProgram = opRoll(program)

    newProgram.stack must be (List(ScriptConstantImpl("14"),
      ScriptConstantImpl("15"), ScriptConstantImpl("16")))
    newProgram.script.isEmpty must be (true)

  }

  it must "evaluate an OP_ROT correctly" in {
    val stack = List(ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"))
    val script = List(OP_ROT)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opRot(program)

    newProgram.stack must be (List(ScriptConstantImpl("16"),ScriptConstantImpl("14"),ScriptConstantImpl("15")))
    newProgram.script.isEmpty must be (true)
  }

  it must "mark the script as invalid if there is less than 3 elements on the stack for OP_ROT" in {
    val stack = List(ScriptConstantImpl("14"), ScriptConstantImpl("15"))
    val script = List(OP_ROT)

    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(opRot(program))
    newProgram.error must be (Some(ScriptErrorInvalidStackOperation))
  }


  it must "evaluate an OP_2ROT correctly" in {
    val stack = List(ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19"))
    val script = List(OP_2ROT)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = op2Rot(program)

    newProgram.stack must be (List(ScriptConstantImpl("18"),ScriptConstantImpl("19"),ScriptConstantImpl("14"),
      ScriptConstantImpl("15"),ScriptConstantImpl("16"), ScriptConstantImpl("17")))
    newProgram.script.isEmpty must be (true)
  }

  it must "mark a scirpt as invalid if there is less than 6 elements on the stack for OP_2ROT" in {
    val stack = List(ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"))
    val script = List(OP_2ROT)

    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(op2Rot(program))
    newProgram.error must be (Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_2DROP correctly" in {
    val stack = List(ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19"))
    val script = List(OP_2DROP)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = op2Drop(program)

    newProgram.stack must be (List(ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19")))
  }


  it must "evaluate an OP_SWAP correctly" in {
    val stack = List(ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19"))
    val script = List(OP_SWAP)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opSwap(program)
    newProgram.stack must be (List(ScriptConstantImpl("15"),ScriptConstantImpl("14"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19")))
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_TUCK correctly" in  {
    val stack = List(ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19"))
    val script = List(OP_TUCK)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opTuck(program)
    newProgram.stack must be (List(ScriptConstantImpl("14"),ScriptConstantImpl("15"),ScriptConstantImpl("14"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19")))
    newProgram.script.isEmpty must be (true)
  }

  it must "mark a script as invalid if there is less than 2 elements on the stack for OP_TUCK" in {
    val stack = List(OP_0)
    val script = List(OP_TUCK)

    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(opTuck(program))
    newProgram.error must be (Some(ScriptErrorInvalidStackOperation))

  }

  it must "evaluate an OP_2DUP correctly" in  {
    val stack = List(ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19"))
    val script = List(OP_2DUP)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = op2Dup(program)
    newProgram.stack must be (List(ScriptConstantImpl("14"),ScriptConstantImpl("15"),
      ScriptConstantImpl("14"),ScriptConstantImpl("15"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19")))
    newProgram.script.isEmpty must be (true)
  }

  it must "mark a script as invalid if there is less than 2 elements on the stack for OP_2DUP" in {
    val stack = List(OP_0)
    val script = List(OP_2DUP)

    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(op2Dup(program))
    newProgram.error must be (Some(ScriptErrorInvalidStackOperation))

  }

  it must "evaluate an OP_3DUP correctly" in {
    val stack = List(ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19"))
    val script = List(OP_3DUP)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = op3Dup(program)

    newProgram.stack must be (List(ScriptConstantImpl("14"),ScriptConstantImpl("15"),ScriptConstantImpl("16"),
      ScriptConstantImpl("14"),ScriptConstantImpl("15"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19")))
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_2OVER correctly" in {
    val stack = List(ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19"))
    val script = List(OP_2OVER)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = op2Over(program)

    newProgram.stack must be (List(ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("14"),ScriptConstantImpl("15"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19")))
    newProgram.script.isEmpty must be (true)
  }

  it must "mark a script as invalid if there is less than 4 elements on the stack for OP_2OVER" in {
    val stack = List(ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"))
    val script = List(OP_2OVER)

    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(op2Over(program))
    newProgram.error must be (Some(ScriptErrorInvalidStackOperation))
  }


  it must "evaluate an OP_2SWAP correctly" in {
    val stack = List(ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"),
      ScriptConstantImpl("17"), ScriptConstantImpl("18"), ScriptConstantImpl("19"))
    val script = List(OP_2SWAP)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = op2Swap(program)

    newProgram.stack must be (List(ScriptConstantImpl("16"), ScriptConstantImpl("17"),ScriptConstantImpl("14"),
      ScriptConstantImpl("15"), ScriptConstantImpl("18"), ScriptConstantImpl("19")))
    newProgram.script.isEmpty must be (true)
  }

  it must "mark a script as invalid if there is less than 4 elements on the stack for OP_2SWAP" in {
    val stack = List(ScriptConstantImpl("14"), ScriptConstantImpl("15"), ScriptConstantImpl("16"))
    val script = List(OP_2SWAP)

    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(op2Swap(program))
    newProgram.error must be (Some(ScriptErrorInvalidStackOperation))
  }
}
