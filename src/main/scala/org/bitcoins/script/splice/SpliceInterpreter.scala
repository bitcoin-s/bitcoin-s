package org.bitcoins.script.splice

import org.bitcoins.script.error.ScriptErrorInvalidStackOperation
import org.bitcoins.script.{ScriptOperationFactory, ScriptProgram}
import org.bitcoins.script.constant._
import Math._

import org.bitcoins.util.BitcoinSLogger

/**
 * Created by chris on 2/4/16.
 */
trait SpliceInterpreter extends BitcoinSLogger {

  /**
   * Pushes the string length of the top element of the stack (without popping it).
 *
   * @param program
   * @return
   */
  def opSize(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_SIZE, "Script top must be OP_SIZE")

    program.stack.size > 0 match {
      case true =>
        if (program.stack.head == OP_0) {
          ScriptProgram(program, OP_0 :: program.stack, program.script.tail)
        } else {
          val scriptNumber = program.stack.head match {
            case ScriptNumber.zero => ScriptNumber.zero
            case x : ScriptToken => ScriptNumber(x.bytes.size)
          }
          ScriptProgram(program, scriptNumber :: program.stack, program.script.tail)
        }
      case false =>
        logger.error("Must have at least 1 element on the stack for OP_SIZE")
        ScriptProgram(program,ScriptErrorInvalidStackOperation)
    }


  }

}
