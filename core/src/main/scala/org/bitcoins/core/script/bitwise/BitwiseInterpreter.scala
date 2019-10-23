package org.bitcoins.core.script.bitwise

import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control.{
  ControlOperationsInterpreter,
  OP_VERIFY
}
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.{
  ExecutedScriptProgram,
  ExecutionInProgressScriptProgram,
  StartedScriptProgram
}
import org.bitcoins.core.util.BitcoinSLogger

/**
  * Created by chris on 1/6/16.
  */
sealed abstract class BitwiseInterpreter {
  private def logger = BitcoinSLogger.logger

  /** Returns 1 if the inputs are exactly equal, 0 otherwise. */
  def opEqual(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_EQUAL),
            "Script operation must be OP_EQUAL")
    if (program.stack.size < 2) {
      program.failExecution(ScriptErrorInvalidStackOperation)
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
      program.updateStackAndScript(scriptBoolean :: program.stack.tail.tail,
                                   program.script.tail)
    }
  }

  /** Same as [[org.bitcoins.core.script.bitwise.OP_EQUAL OP_EQUAL]], but runs
    * [[org.bitcoins.core.script.control.OP_VERIFY OP_VERIFY]] afterward. */
  def opEqualVerify(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_EQUALVERIFY),
            "Script operation must be OP_EQUALVERIFY")
    if (program.stack.size > 1) {
      //first replace OP_EQUALVERIFY with OP_EQUAL and OP_VERIFY
      val simpleScript = OP_EQUAL :: OP_VERIFY :: program.script.tail
      val newProgram = opEqual(program.updateScript(simpleScript))
      val verifiedOrErr = newProgram match {
        case err: ExecutedScriptProgram => err
        case p: ExecutionInProgressScriptProgram =>
          ControlOperationsInterpreter.opVerify(p)
      }

      verifiedOrErr match {
        case p: ExecutedScriptProgram =>
          if (p.error.isDefined) p.failExecution(ScriptErrorEqualVerify)
          else p
        case p: ExecutionInProgressScriptProgram => p
      }
    } else {
      logger.error("OP_EQUALVERIFY requires at least 2 elements on the stack")
      program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }
}

object BitwiseInterpreter extends BitwiseInterpreter
