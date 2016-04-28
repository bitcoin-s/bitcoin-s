package org.bitcoins.util

import org.bitcoins.script.{ExecutionInProgressScriptProgram, PreExecutionScriptProgram, ExecutedScriptProgram, ScriptProgram}

/**
 * Created by chris on 4/20/16.
 */
trait ScriptProgramTestUtil {

  /**
   * Matches a ScriptProgram to an ExecutedScriptProgram or else throws an exception
   * useful for testing purposes
 *
   * @param p
   * @return
   */
  def toExecutedScriptProgram(p : ScriptProgram) : ExecutedScriptProgram = p match {
    case e : ExecutedScriptProgram => e
    case _ : PreExecutionScriptProgram | _ : ExecutionInProgressScriptProgram =>
      throw new RuntimeException("Should be a executed script proram")
  }

  /**
   * Matches a ScriptProgram to a PreExecutionScriptProgram or else throws an exception
   * useful to for test purposes
 *
   * @param p
   * @return
   */
  def toPreExecutionScriptProgram(p : ScriptProgram) : PreExecutionScriptProgram = p match {
    case e : PreExecutionScriptProgram => e
    case _ : ExecutionInProgressScriptProgram | _ : ExecutedScriptProgram =>
      throw new RuntimeException("Must be a pre executed scirpt program")
  }

  /**
   * Matches a ScriptProgram to a ExecutionInProgressScriptProgram or else throws an exception
 *
   * @param p
   * @return
   */
  def toExecutionInProgressScriptProgram(p : ScriptProgram) : ExecutionInProgressScriptProgram = p match {
    case e : ExecutionInProgressScriptProgram => e
    case _ : PreExecutionScriptProgram | _ : ExecutedScriptProgram =>
      throw new RuntimeException("Must be a execution in progress script program")
  }
}


object ScriptProgramTestUtil extends ScriptProgramTestUtil