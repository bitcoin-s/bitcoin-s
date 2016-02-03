package org.scalacoin.script.bitwise

import org.scalacoin.script.{ScriptProgramImpl, ScriptProgram}
import org.scalacoin.script.constant.{ScriptFalse, ScriptTrue, ScriptConstantImpl, ScriptToken}
import org.scalacoin.script.control.{OP_VERIFY, ControlOperationsInterpreter}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/6/16.
 */
trait BitwiseInterpreter extends ControlOperationsInterpreter  {

  private def logger = LoggerFactory.getLogger(this.getClass())

  /**
   * Returns 1 if the inputs are exactly equal, 0 otherwise.
   * @param program
   * @return
   */
  def equal(program : ScriptProgram) : ScriptProgram = {
    require(program.stack.size > 1, "Stack size must be 2 or more to compare the top two values for OP_EQUAL")
    require(program.script.headOption.isDefined && program.script.head == OP_EQUAL, "Script operation must be OP_EQUAL")

    logger.debug("Original stack: " + program.stack)
    val newStack = program.stack match {
      case h :: h1 :: t => if (h == h1) ScriptTrue :: t else ScriptFalse  :: t
    }
    logger.debug("New Stack: " + newStack)
    ScriptProgramImpl(newStack,program.script.tail,program.transaction)
  }

  /**
   * Same as OP_EQUAL, but runs OP_VERIFY afterward.
   * @param stack
   * @param script
   * @return
   */
  def equalVerify(program : ScriptProgram) : ScriptProgram = {
    require(program.stack.size > 1, "Stack size must be 2 or more to compare the top two values")
    require(program.script.headOption.isDefined && program.script.head == OP_EQUALVERIFY, "Script operation must be OP_EQUALVERIFY")
    //first replace OP_EQUALVERIFY with OP_EQUAL and OP_VERIFY
    val simpleScript = OP_EQUAL :: OP_VERIFY :: program.script.tail
    val newProgram: ScriptProgram = equal(ScriptProgramImpl(program.stack,simpleScript,program.transaction))
    opVerify(newProgram)
  }


}
