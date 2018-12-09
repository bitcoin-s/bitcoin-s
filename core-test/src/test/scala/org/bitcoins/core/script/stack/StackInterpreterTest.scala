package org.bitcoins.core.script.stack

import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.{ExecutedScriptProgram, ScriptProgram}
import org.bitcoins.core.util.{BitcoinSUtil, ScriptProgramTestUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 1/6/16.
  */
class StackInterpreterTest extends FlatSpec with MustMatchers {
  val element1 = ScriptConstant("1234")
  val element2 = ScriptConstant("abcd")
  val stack = List(element1, element2)
  val SI = StackInterpreter
  "StackInterpreter" must "duplicate elements on top of the stack" in {

    val script = List(OP_DUP)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.opDup(program)

    newProgram.stack.head must be(element1)
    newProgram.stack(1) must be(element1)
    newProgram.stack(2) must be(element2)
  }

  it must "throw an exception when calling opDup without an OP_DUP on top of the script stack" in {
    intercept[IllegalArgumentException] {
      val script = List()
      val program = ScriptProgram(TestUtil.testProgram, stack, script)
      SI.opDup(program)
    }
  }

  it must "mark the script invalid when calling opDup without an element on top of the stack" in {
    val stack = List()
    val script = List(OP_DUP)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    ScriptProgramTestUtil
      .toExecutedScriptProgram(SI.opDup(program))
      .error must be(Some(ScriptErrorInvalidStackOperation))

  }

  it must "evaluate the OP_DEPTH operator correctly" in {
    val script = List(OP_DEPTH)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.opDepth(program)

    newProgram.stack.head.hex must be(BitcoinSUtil.encodeHex(stack.size.toByte))
  }

  it must "evaluate OP_DEPTH operator correctly when there are zero items on the stack" in {
    val stack = List()
    val script = List(OP_DEPTH)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.opDepth(program)
    newProgram.stack.head must be(ScriptNumber.zero)
  }

  it must "evaluate an OP_TOALTSTACK operator correctly" in {
    val stack = List(OP_0)
    val script = List(OP_TOALTSTACK)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.opToAltStack(program)

    newProgram.stack.isEmpty must be(true)
    newProgram.script.isEmpty must be(true)
    newProgram.altStack must be(List(OP_0))

  }

  it must "evaluate an OP_DROP operator correctly" in {
    val stack = List(OP_0)
    val script = List(OP_DROP)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.opDrop(program)

    newProgram.stack.isEmpty must be(true)
    newProgram.script.isEmpty must be(true)
  }

  it must "mark the script as invalid for an OP_DROP if we do not have a stack element" in {
    val stack = List()
    val script = List(OP_DROP)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = SI.opDrop(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_IFDUP correctly" in {
    val stack = List(ScriptNumber.zero)
    val script = List(OP_IFDUP)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.opIfDup(program)

    newProgram.stack must be(stack)
    newProgram.script.isEmpty must be(true)

    val stack1 = List(OP_1)
    val program1 = ScriptProgram(TestUtil.testProgram, stack1, script)
    val newProgram1 = SI.opIfDup(program1)
    newProgram1.stack must be(List(OP_1, OP_1))
    newProgram1.script.isEmpty must be(true)

  }

  it must "evaluate an OP_NIP correctly" in {
    val stack = List(OP_0, OP_1)
    val script = List(OP_NIP)

    val program = ScriptProgram(TestUtil.testProgram, stack, script)

    val newProgram = SI.opNip(program)

    newProgram.stack must be(List(OP_0))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark the script as invalid if there is less than 2 elements on the stack for OP_NIP" in {
    val stack = List(OP_0)
    val script = List(OP_NIP)

    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(SI.opNip(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))

  }

  it must "throw an exception if there is no elements on the stack for OP_NIP" in {
    val stack = List()
    val script = List(OP_NIP)

    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(SI.opNip(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_OVER correctly" in {
    val stack = List(OP_0, OP_1)
    val script = List(OP_OVER)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.opOver(program)
    newProgram.stack must be(List(OP_1, OP_0, OP_1))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark the script as invalid if there is less than 2 elements on the stack for OP_OVER" in {
    val stack = List(OP_0)
    val script = List(OP_OVER)

    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(SI.opOver(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))

  }

  it must "mark the script as invalid if there is no elements on the stack for OP_OVER" in {
    val stack = List()
    val script = List(OP_OVER)

    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(SI.opOver(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))

  }

  it must "evaluate an OP_PICK correctly" in {
    val stack = List(ScriptNumber.zero,
                     ScriptConstant("14"),
                     ScriptConstant("15"),
                     ScriptConstant("16"))
    val script = List(OP_PICK)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.opPick(program)

    newProgram.stack must be(
      List(ScriptConstant("14"),
           ScriptConstant("14"),
           ScriptConstant("15"),
           ScriptConstant("16")))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark the script as invalid for OP_PICK if we do not have enough items on the stack for the first number" in {
    val stack = List(ScriptNumber.one)
    val script = List(OP_PICK)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = SI.opPick(program)

    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))

  }
  it must "evaluate an OP_ROLL correctly" in {
    val stack = List(ScriptNumber.zero,
                     ScriptConstant("14"),
                     ScriptConstant("15"),
                     ScriptConstant("16"))
    val script = List(OP_ROLL)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = SI.opRoll(program)

    newProgram.stack must be(
      List(ScriptConstant("14"), ScriptConstant("15"), ScriptConstant("16")))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark an OP_ROLL as invalid if we do not have the enough stack elements indicated by the stack top" in {
    val stack = List(ScriptNumber.one)
    val script = List(OP_ROLL)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = SI.opRoll(program)

    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))
  }

  it must "mark an OP_ROLL as invalid if the script number is not minimall encoded" in {
    val stack = List(ScriptNumber("0100"))
    val script = List(OP_ROLL)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = SI.opRoll(program)

    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_ROT correctly" in {
    val stack =
      List(ScriptConstant("14"), ScriptConstant("15"), ScriptConstant("16"))
    val script = List(OP_ROT)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.opRot(program)

    newProgram.stack must be(
      List(ScriptConstant("16"), ScriptConstant("14"), ScriptConstant("15")))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark the script as invalid if there is less than 3 elements on the stack for OP_ROT" in {
    val stack = List(ScriptConstant("14"), ScriptConstant("15"))
    val script = List(OP_ROT)

    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(SI.opRot(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_2ROT correctly" in {
    val stack = List(ScriptConstant("14"),
                     ScriptConstant("15"),
                     ScriptConstant("16"),
                     ScriptConstant("17"),
                     ScriptConstant("18"),
                     ScriptConstant("19"))
    val script = List(OP_2ROT)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.op2Rot(program)

    newProgram.stack must be(
      List(ScriptConstant("18"),
           ScriptConstant("19"),
           ScriptConstant("14"),
           ScriptConstant("15"),
           ScriptConstant("16"),
           ScriptConstant("17")))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark a scirpt as invalid if there is less than 6 elements on the stack for OP_2ROT" in {
    val stack = List(ScriptConstant("14"),
                     ScriptConstant("15"),
                     ScriptConstant("16"),
                     ScriptConstant("17"),
                     ScriptConstant("18"))
    val script = List(OP_2ROT)

    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(SI.op2Rot(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_2DROP correctly" in {
    val stack = List(ScriptConstant("14"),
                     ScriptConstant("15"),
                     ScriptConstant("16"),
                     ScriptConstant("17"),
                     ScriptConstant("18"),
                     ScriptConstant("19"))
    val script = List(OP_2DROP)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.op2Drop(program)

    newProgram.stack must be(
      List(ScriptConstant("16"),
           ScriptConstant("17"),
           ScriptConstant("18"),
           ScriptConstant("19")))
  }

  it must "mark a script invalid if an OP_2DROP script does not have two stack items" in {
    val stack = List(ScriptConstant("14"))
    val script = List(OP_2DROP)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = SI.op2Drop(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_SWAP correctly" in {
    val stack = List(ScriptConstant("14"),
                     ScriptConstant("15"),
                     ScriptConstant("16"),
                     ScriptConstant("17"),
                     ScriptConstant("18"),
                     ScriptConstant("19"))
    val script = List(OP_SWAP)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.opSwap(program)
    newProgram.stack must be(
      List(ScriptConstant("15"),
           ScriptConstant("14"),
           ScriptConstant("16"),
           ScriptConstant("17"),
           ScriptConstant("18"),
           ScriptConstant("19")))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark a script invalid if an OP_SWAP script does not have two stack items" in {
    val stack = List(ScriptConstant("14"))
    val script = List(OP_SWAP)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = SI.opSwap(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_TUCK correctly" in {
    val stack = List(ScriptConstant("14"),
                     ScriptConstant("15"),
                     ScriptConstant("16"),
                     ScriptConstant("17"),
                     ScriptConstant("18"),
                     ScriptConstant("19"))
    val script = List(OP_TUCK)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.opTuck(program)
    newProgram.stack must be(
      List(ScriptConstant("14"),
           ScriptConstant("15"),
           ScriptConstant("14"),
           ScriptConstant("16"),
           ScriptConstant("17"),
           ScriptConstant("18"),
           ScriptConstant("19")))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark a script as invalid if there is less than 2 elements on the stack for OP_TUCK" in {
    val stack = List(OP_0)
    val script = List(OP_TUCK)

    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(SI.opTuck(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))

  }

  it must "evaluate an OP_2DUP correctly" in {
    val stack = List(ScriptConstant("14"),
                     ScriptConstant("15"),
                     ScriptConstant("16"),
                     ScriptConstant("17"),
                     ScriptConstant("18"),
                     ScriptConstant("19"))
    val script = List(OP_2DUP)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.op2Dup(program)
    newProgram.stack must be(
      List(
        ScriptConstant("14"),
        ScriptConstant("15"),
        ScriptConstant("14"),
        ScriptConstant("15"),
        ScriptConstant("16"),
        ScriptConstant("17"),
        ScriptConstant("18"),
        ScriptConstant("19")
      ))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark a script as invalid if there is less than 2 elements on the stack for OP_2DUP" in {
    val stack = List(OP_0)
    val script = List(OP_2DUP)

    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(SI.op2Dup(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))

  }

  it must "evaluate an OP_3DUP correctly" in {
    val stack = List(ScriptConstant("14"),
                     ScriptConstant("15"),
                     ScriptConstant("16"),
                     ScriptConstant("17"),
                     ScriptConstant("18"),
                     ScriptConstant("19"))
    val script = List(OP_3DUP)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.op3Dup(program)

    newProgram.stack must be(
      List(
        ScriptConstant("14"),
        ScriptConstant("15"),
        ScriptConstant("16"),
        ScriptConstant("14"),
        ScriptConstant("15"),
        ScriptConstant("16"),
        ScriptConstant("17"),
        ScriptConstant("18"),
        ScriptConstant("19")
      ))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark the script as invalid if OP_3DUP does not have three stack elements" in {
    val stack = List(ScriptConstant("14"), ScriptConstant("15"))
    val script = List(OP_3DUP)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = SI.op3Dup(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_2OVER correctly" in {
    val stack = List(ScriptConstant("14"),
                     ScriptConstant("15"),
                     ScriptConstant("16"),
                     ScriptConstant("17"),
                     ScriptConstant("18"),
                     ScriptConstant("19"))
    val script = List(OP_2OVER)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.op2Over(program)

    newProgram.stack must be(
      List(
        ScriptConstant("16"),
        ScriptConstant("17"),
        ScriptConstant("14"),
        ScriptConstant("15"),
        ScriptConstant("16"),
        ScriptConstant("17"),
        ScriptConstant("18"),
        ScriptConstant("19")
      ))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark a script as invalid if there is less than 4 elements on the stack for OP_2OVER" in {
    val stack =
      List(ScriptConstant("14"), ScriptConstant("15"), ScriptConstant("16"))
    val script = List(OP_2OVER)

    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(SI.op2Over(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_2SWAP correctly" in {
    val stack = List(ScriptConstant("14"),
                     ScriptConstant("15"),
                     ScriptConstant("16"),
                     ScriptConstant("17"),
                     ScriptConstant("18"),
                     ScriptConstant("19"))
    val script = List(OP_2SWAP)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = SI.op2Swap(program)

    newProgram.stack must be(
      List(ScriptConstant("16"),
           ScriptConstant("17"),
           ScriptConstant("14"),
           ScriptConstant("15"),
           ScriptConstant("18"),
           ScriptConstant("19")))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark a script as invalid if there is less than 4 elements on the stack for OP_2SWAP" in {
    val stack =
      List(ScriptConstant("14"), ScriptConstant("15"), ScriptConstant("16"))
    val script = List(OP_2SWAP)

    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(SI.op2Swap(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))
  }

  it must "move an element from the alt stack to the main stack" in {
    val stack = List()
    val script = List(OP_FROMALTSTACK)
    val altStack = List(OP_0)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val programWithAltStack =
      ScriptProgram(program, altStack, ScriptProgram.AltStack)
    val executedProgram = SI.opFromAltStack(programWithAltStack)
    executedProgram.stack must be(altStack)
  }
}
