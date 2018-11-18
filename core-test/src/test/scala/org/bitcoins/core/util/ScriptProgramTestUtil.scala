package org.bitcoins.core.util

import org.bitcoins.core.script.{ ExecutedScriptProgram, ExecutionInProgressScriptProgram, PreExecutionScriptProgram, ScriptProgram }

/**
 * Created by chris on 4/20/16.
 */
trait ScriptProgramTestUtil {

  /**
   * Matches a [[ScriptProgram]] to an [[ExecutedScriptProgram]] or else throws an exception
   * useful for testing purposes.
   */
  def toExecutedScriptProgram(p: ScriptProgram): ExecutedScriptProgram = p match {
    case e: ExecutedScriptProgram => e
    case _: PreExecutionScriptProgram | _: ExecutionInProgressScriptProgram =>
      throw new RuntimeException("Should be an executed script program")
  }

  /**
   * Matches a [[ScriptProgram]] to a [[PreExecutionScriptProgram]] or else throws an exception
   * useful for test purposes.
   */
  def toPreExecutionScriptProgram(p: ScriptProgram): PreExecutionScriptProgram = p match {
    case e: PreExecutionScriptProgram => e
    case _: ExecutionInProgressScriptProgram | _: ExecutedScriptProgram =>
      throw new RuntimeException("Must be a pre executed script program")
  }

  /** Matches a [[ScriptProgram]] to a [[ExecutionInProgressScriptProgram]] or else throws an exception.*/
  def toExecutionInProgressScriptProgram(p: ScriptProgram): ExecutionInProgressScriptProgram = p match {
    case e: ExecutionInProgressScriptProgram => e
    case _: PreExecutionScriptProgram | _: ExecutedScriptProgram =>
      throw new RuntimeException("Must be an execution in progress script program")
  }
}

object ScriptProgramTestUtil extends ScriptProgramTestUtil