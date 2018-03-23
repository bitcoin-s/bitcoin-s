package org.bitcoins.core.script.bitwise

import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control.{ ControlOperationsInterpreter, OP_VERIFY }
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.{ ExecutedScriptProgram, ExecutionInProgressScriptProgram, PreExecutionScriptProgram, ScriptProgram }
import org.bitcoins.core.util.BitcoinSLogger

/**
 * Created by chris on 1/6/16.
 */
sealed abstract class BitwiseInterpreter {
  private def logger = BitcoinSLogger.logger
  /** Returns 1 if the inputs are exactly equal, 0 otherwise. */
  def opEqual(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_EQUAL), "Script operation must be OP_EQUAL")
    if (program.stack.size < 2) {
      ScriptProgram(program, ScriptErrorInvalidStackOperation)
    } else {
      val h = program.stack.head
      val h1 = program.stack.tail.head
      val result = (h, h1) match {
        case (OP_0, ScriptNumber.zero) | (ScriptNumber.zero, OP_0) =>
          OP_0.underlying == ScriptNumber.zero.toLong
        case (OP_FALSE, ScriptNumber.zero) | (ScriptNumber.zero, OP_FALSE) =>
          OP_FALSE.underlying == ScriptNumber.zero.toLong
        case (OP_TRUE, ScriptNumber.one) | (ScriptNumber.one, OP_TRUE) =>
          OP_TRUE.underlying == ScriptNumber.one.toLong
        case (OP_1, ScriptNumber.one) | (ScriptNumber.one, OP_1) =>
          OP_1.underlying == ScriptNumber.one.toLong
        case _ => h.bytes == h1.bytes
      }
      val scriptBoolean = if (result) OP_TRUE else OP_FALSE
      ScriptProgram(program, scriptBoolean :: program.stack.tail.tail, program.script.tail)
    }
  }

  /** Same as [[OP_EQUAL]], but runs [[OP_VERIFY]] afterward. */
  def opEqualVerify(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_EQUALVERIFY), "Script operation must be OP_EQUALVERIFY")
    if (program.stack.size > 1) {
      //first replace OP_EQUALVERIFY with OP_EQUAL and OP_VERIFY
      val simpleScript = OP_EQUAL :: OP_VERIFY :: program.script.tail
      val newProgram: ScriptProgram = opEqual(ScriptProgram(program, program.stack, simpleScript))
      ControlOperationsInterpreter.opVerify(newProgram) match {
        case p: PreExecutionScriptProgram => p
        case p: ExecutedScriptProgram =>
          if (p.error.isDefined) ScriptProgram(p, ScriptErrorEqualVerify)
          else p
        case p: ExecutionInProgressScriptProgram => p
      }
    } else {
      logger.error("OP_EQUALVERIFY requires at least 2 elements on the stack")
      ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }
}

object BitwiseInterpreter extends BitwiseInterpreter