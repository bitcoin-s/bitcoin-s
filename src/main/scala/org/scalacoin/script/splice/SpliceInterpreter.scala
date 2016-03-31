package org.scalacoin.script.splice

import org.scalacoin.script.{ScriptProgramFactory, ScriptOperationFactory, ScriptProgramImpl, ScriptProgram}
import org.scalacoin.script.constant._
import Math._
/**
 * Created by chris on 2/4/16.
 */
trait SpliceInterpreter {

  /**
   * Pushes the string length of the top element of the stack (without popping it).
   * @param program
   * @return
   */
  def opSize(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_SIZE, "Script top must be OP_SIZE")
    require(program.stack.size > 0, "Must have at least 1 element on the stack for OP_SIZE")
    if (program.stack.head == OP_0) {
      ScriptProgramFactory.factory(program, OP_0 :: program.stack, program.script.tail)
    } else {
      val scriptNumber = program.stack.head match {
        case OP_0 => ScriptNumberImpl(0)
        case ScriptNumberImpl(0) => ScriptNumberImpl(0)
        case x : ScriptToken => ScriptNumberImpl(x.hex.size / 2)
      }
      ScriptProgramFactory.factory(program, scriptNumber :: program.stack, program.script.tail)
    }

  }

  /**
   * Finds how many bytes are in a number
   * @param x
   * @return
   */
  private def bytes(x: Long): Int = {
    val posx = x.abs
    if (posx == 0L) 0
    else (64 - java.lang.Long.numberOfLeadingZeros(posx)) / 8 + 1
  }
}
