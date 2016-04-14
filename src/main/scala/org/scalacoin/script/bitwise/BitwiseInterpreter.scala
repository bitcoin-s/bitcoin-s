package org.scalacoin.script.bitwise

import org.scalacoin.script.{ScriptProgramFactory, ScriptProgram}
import org.scalacoin.script.constant._
import org.scalacoin.script.control.{OP_VERIFY, ControlOperationsInterpreter}
import org.scalacoin.util.BitcoinSUtil
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/6/16.
 */
trait BitwiseInterpreter extends ControlOperationsInterpreter  {

  /**
   * Returns 1 if the inputs are exactly equal, 0 otherwise.
   * @param program
   * @return
   */
  def opEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_EQUAL, "Script operation must be OP_EQUAL")

    if (program.stack.size < 2) {
      ScriptProgramFactory.factory(program,false)
    } else {
      val h = program.stack.head
      val h1 = program.stack.tail.head
      val result = (h,h1) match {
        case (OP_0,ScriptNumberFactory.zero) | (ScriptNumberFactory.zero, OP_0) =>
          OP_0.num == ScriptNumberFactory.zero.num
        case (OP_FALSE,ScriptNumberFactory.zero) | (ScriptNumberFactory.zero, OP_FALSE) =>
          OP_FALSE.num == ScriptNumberFactory.zero.num
        case (OP_TRUE,ScriptNumberFactory.one) | (ScriptNumberFactory.one, OP_TRUE) =>
          OP_TRUE.num == ScriptNumberFactory.one.num
        case (OP_1, ScriptNumberFactory.one) | (ScriptNumberFactory.one, OP_1) =>
          OP_1.num == ScriptNumberFactory.one.num
        case (ScriptFalse, ScriptNumberFactory.zero) | (ScriptNumberFactory.zero, ScriptFalse) =>
          ScriptFalse.num == ScriptNumberFactory.zero.num
        case (ScriptFalse, OP_0) | (OP_0, ScriptFalse) =>
          ScriptFalse.num == OP_0.num
        case (ScriptTrue, ScriptNumberFactory.one) | (ScriptNumberFactory.one, ScriptTrue) =>
          ScriptTrue.num == ScriptNumberFactory.one.num
        case (ScriptTrue, OP_1) | (OP_1, ScriptTrue) =>
          ScriptTrue.num == OP_1.num
        case _ => h.bytes == h1.bytes
      }
      val scriptBoolean : ScriptBoolean = if (result) ScriptTrue else ScriptFalse
      ScriptProgramFactory.factory(program,scriptBoolean :: program.stack.tail.tail, program.script.tail)
    }

  }


  /**
   * Same as OP_EQUAL, but runs OP_VERIFY afterward.
   * @param program
   * @return
   */
  def opEqualVerify(program : ScriptProgram) : ScriptProgram = {
    require(program.stack.size > 1, "Stack size must be 2 or more to compare the top two values")
    require(program.script.headOption.isDefined && program.script.head == OP_EQUALVERIFY, "Script operation must be OP_EQUALVERIFY")
    //first replace OP_EQUALVERIFY with OP_EQUAL and OP_VERIFY
    val simpleScript = OP_EQUAL :: OP_VERIFY :: program.script.tail
    val newProgram: ScriptProgram = opEqual(ScriptProgramFactory.factory(program, program.stack, simpleScript))
    opVerify(newProgram)
  }


}
