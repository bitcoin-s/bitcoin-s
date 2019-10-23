package org.bitcoins.core.util

import org.bitcoins.core.script.{
  ExecutedScriptProgram,
  ExecutionInProgressScriptProgram,
  PreExecutionScriptProgram,
  StartedScriptProgram
}

/**
  * Created by chris on 4/20/16.
  */
trait ScriptProgramTestUtil {

  /**
    * Matches a [[StartedScriptProgram]] to an [[ExecutedScriptProgram]] or else throws an exception
    * useful for testing purposes.
    */
  def toExecutedScriptProgram(p: StartedScriptProgram): ExecutedScriptProgram =
    p match {
      case e: ExecutedScriptProgram => e
      case _: PreExecutionScriptProgram | _: ExecutionInProgressScriptProgram =>
        throw new RuntimeException("Should be an executed script program")
    }

  /** Matches a [[StartedScriptProgram]] to a [[ExecutionInProgressScriptProgram]] or else throws an exception.*/
  def toExecutionInProgressScriptProgram(
      p: StartedScriptProgram): ExecutionInProgressScriptProgram = p match {
    case e: ExecutionInProgressScriptProgram => e
    case _: PreExecutionScriptProgram | _: ExecutedScriptProgram =>
      throw new RuntimeException(
        "Must be an execution in progress script program")
  }
}

object ScriptProgramTestUtil extends ScriptProgramTestUtil
