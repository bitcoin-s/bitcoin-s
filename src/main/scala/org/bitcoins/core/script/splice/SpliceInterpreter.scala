package org.bitcoins.core.script.splice

import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.result.ScriptErrorInvalidStackOperation
import org.bitcoins.core.util.BitcoinSLogger

/**
 * Created by chris on 2/4/16.
 */
sealed abstract class SpliceInterpreter {

  private def logger = BitcoinSLogger.logger
  /** Pushes the string length of the top element of the stack (without popping it). */
  def opSize(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_SIZE), "Script top must be OP_SIZE")
    if (program.stack.nonEmpty) {
      if (program.stack.head == OP_0) {
        ScriptProgram(program, OP_0 :: program.stack, program.script.tail)
      } else {
        val scriptNumber = program.stack.head match {
          case ScriptNumber.zero => ScriptNumber.zero
          case x: ScriptToken    => ScriptNumber(x.bytes.size)
        }
        ScriptProgram(program, scriptNumber :: program.stack, program.script.tail)
      }
    } else {
      logger.error("Must have at least 1 element on the stack for OP_SIZE")
      ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }
}

object SpliceInterpreter extends SpliceInterpreter