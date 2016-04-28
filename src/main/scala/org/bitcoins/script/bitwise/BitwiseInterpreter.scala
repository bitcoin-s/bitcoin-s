package org.bitcoins.script.bitwise

import org.bitcoins.script.error.ScriptErrorInvalidStackOperation
import org.bitcoins.script.{ScriptProgram}
import org.bitcoins.script.constant._
import org.bitcoins.script.control.{OP_VERIFY, ControlOperationsInterpreter}
import org.bitcoins.util.BitcoinSUtil
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/6/16.
 */
trait BitwiseInterpreter extends ControlOperationsInterpreter  {

  /**
   * Returns 1 if the inputs are exactly equal, 0 otherwise.
 *
   * @param program
   * @return
   */
  def opEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_EQUAL, "Script operation must be OP_EQUAL")

    if (program.stack.size < 2) {
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    } else {
      val h = program.stack.head
      val h1 = program.stack.tail.head
      val result = (h,h1) match {
        case (OP_0,ScriptNumber.zero) | (ScriptNumber.zero, OP_0) =>
          OP_0.num == ScriptNumber.zero.num
        case (OP_FALSE,ScriptNumber.zero) | (ScriptNumber.zero, OP_FALSE) =>
          OP_FALSE.num == ScriptNumber.zero.num
        case (OP_TRUE,ScriptNumber.one) | (ScriptNumber.one, OP_TRUE) =>
          OP_TRUE.num == ScriptNumber.one.num
        case (OP_1, ScriptNumber.one) | (ScriptNumber.one, OP_1) =>
          OP_1.num == ScriptNumber.one.num
        case (OP_0,ScriptNumber.zero) | (ScriptNumber.zero, OP_0) =>
          OP_0.num == ScriptNumber.zero.num
        case (OP_FALSE,ScriptNumber.zero) | (ScriptNumber.zero, OP_FALSE) =>
          OP_FALSE.num == ScriptNumber.zero.num
        case (OP_TRUE,ScriptNumber.one) | (ScriptNumber.one, OP_TRUE) =>
          OP_TRUE.num == ScriptNumber.one.num
        case (OP_1, ScriptNumber.one) | (ScriptNumber.one, OP_1) =>
          OP_1.num == ScriptNumber.one.num
        case _ => h.bytes == h1.bytes
      }
      val scriptBoolean  = if (result) OP_TRUE else OP_FALSE
      ScriptProgram(program,scriptBoolean :: program.stack.tail.tail, program.script.tail)
    }

  }


  /**
   * Same as OP_EQUAL, but runs OP_VERIFY afterward.
 *
   * @param program
   * @return
   */
  def opEqualVerify(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_EQUALVERIFY, "Script operation must be OP_EQUALVERIFY")
    program.stack.size > 1 match {
      case true =>
        //first replace OP_EQUALVERIFY with OP_EQUAL and OP_VERIFY
        val simpleScript = OP_EQUAL :: OP_VERIFY :: program.script.tail
        val newProgram: ScriptProgram = opEqual(ScriptProgram(program, program.stack, simpleScript))
        opVerify(newProgram)
      case false =>
        logger.error("OP_EQUALVERIFY requires at least 2 elements on the stack")
        ScriptProgram(program,ScriptErrorInvalidStackOperation)
    }
  }
}
