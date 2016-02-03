package org.scalacoin.script.bitwise

import org.scalacoin.script.{ScriptProgramImpl, ScriptProgram}
import org.scalacoin.script.constant._
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
    val h = program.stack.head
    val h1 = program.stack.tail.head

    val result = (h,h1) match {
      case (ScriptConstantImpl(x),ScriptConstantImpl(y)) => x == y
      case (ScriptNumberImpl(x), ScriptNumberImpl(y)) => x == y
      case (ScriptNumberImpl(x), ScriptConstantImpl(y)) => ScriptNumberImpl(x).hex == y
      case (ScriptConstantImpl(x), ScriptNumberImpl(y)) => x == ScriptNumberImpl(y).hex
      case _ => h == h1
    }

    val scriptBoolean = if (result) ScriptTrue else ScriptFalse

    ScriptProgramImpl(scriptBoolean :: program.stack.tail.tail,program.script.tail,program.transaction, program.altStack)
  }


  /**
   * Same as OP_EQUAL, but runs OP_VERIFY afterward.
   * @param program
   * @return
   */
  def equalVerify(program : ScriptProgram) : ScriptProgram = {
    require(program.stack.size > 1, "Stack size must be 2 or more to compare the top two values")
    require(program.script.headOption.isDefined && program.script.head == OP_EQUALVERIFY, "Script operation must be OP_EQUALVERIFY")
    //first replace OP_EQUALVERIFY with OP_EQUAL and OP_VERIFY
    val simpleScript = OP_EQUAL :: OP_VERIFY :: program.script.tail
    val newProgram: ScriptProgram = equal(ScriptProgramImpl(program.stack,
      simpleScript,program.transaction, program.altStack))
    opVerify(newProgram)
  }


}
