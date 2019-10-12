package org.bitcoins.core.script

import org.bitcoins.core.crypto._
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.script.result._
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}

/**
  * Created by chris on 2/3/16.
  */
sealed trait ScriptProgram {

  /**
    * This contains all relevant information for hashing and checking a
    * [[org.bitcoins.core.protocol.script.ScriptSignature ScriptSignature]] for
    * a [[org.bitcoins.core.protocol.transaction.Transaction Transaction]].
    */
  def txSignatureComponent: TxSigComponent

  /** The current state of the stack for execution of the
    * [[org.bitcoins.core.script.ScriptProgram ScriptProgram]]. */
  def stack: List[ScriptToken]

  /** The script operations that need to still be executed. */
  def script: List[ScriptToken]

  /** The original script that was given. */
  def originalScript: List[ScriptToken]

  /** The alternative stack is used in some Script op codes. */
  def altStack: List[ScriptToken]

  /**
    * [[org.bitcoins.core.script.flag.ScriptFlag ScriptFlag]] that are run with the script.
    * These flags indicate special conditions that a script needs to be run with.
    * [[https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.h#L31]]
    * @return
    */
  def flags: Seq[ScriptFlag]

  /** Returns true if the stack top is true */
  def stackTopIsTrue: Boolean =
    stack.nonEmpty && BitcoinScriptUtil.castToBool(stack.head)

  /** Returns true if the stack top is false */
  def stackTopIsFalse: Boolean = !stackTopIsTrue

  /**
    * Sets a [[org.bitcoins.core.script.result.ScriptError ScriptError]] on a given
    * [[org.bitcoins.core.script.ScriptProgram ScriptProgram]].
    * @param error the error that the program hit while being executed in the script interpreter
    * @return the ExecutedScriptProgram with the given error set inside of the trait
    */
  def failExecution(error: ScriptError): ExecutedScriptProgram
}

/**
  * This represents a [[org.bitcoins.core.script.ScriptProgram ScriptProgram]]
  * before any script operations have been executed in the
  * [[org.bitcoins.core.script.interpreter.ScriptInterpreter ScriptInterpreter]].
  */
case class PreExecutionScriptProgram(
    txSignatureComponent: TxSigComponent,
    stack: List[ScriptToken],
    script: List[ScriptToken],
    originalScript: List[ScriptToken],
    altStack: List[ScriptToken],
    flags: Seq[ScriptFlag])
    extends ScriptProgram {
  override def failExecution(error: ScriptError): ExecutedScriptProgram = {
    ScriptProgram.toExecutionInProgress(this).failExecution(error)
  }

  def updateStack(tokens: Seq[ScriptToken]): PreExecutionScriptProgram = {
    this.copy(stack = tokens.toList)
  }

  def updateAltStack(tokens: Seq[ScriptToken]): PreExecutionScriptProgram = {
    this.copy(altStack = tokens.toList)
  }

  def updateScript(tokens: Seq[ScriptToken]): PreExecutionScriptProgram = {
    this.copy(script = tokens.toList)
  }

  def updateOriginalScript(
      tokens: Seq[ScriptToken]): PreExecutionScriptProgram = {
    this.copy(originalScript = tokens.toList)
  }
}

object PreExecutionScriptProgram {

  def apply(txSigComponent: TxSigComponent): PreExecutionScriptProgram = {
    PreExecutionScriptProgram(
      txSignatureComponent = txSigComponent,
      stack = Nil,
      script = txSigComponent.scriptSignature.asm.toList,
      originalScript = txSigComponent.scriptSignature.asm.toList,
      altStack = Nil,
      flags = txSigComponent.flags
    )
  }
}

/** This represents any ScriptProgram that is not PreExecution */
sealed trait StartedScriptProgram extends ScriptProgram

/**
  * Type for a [[org.bitcoins.core.script.ScriptProgram ScriptProgram]] that is currently being
  * evaluated by the [[org.bitcoins.core.script.interpreter.ScriptInterpreter ScriptInterpreter]].
  *
  * @param lastCodeSeparator The index of the last [[org.bitcoins.core.script.crypto.OP_CODESEPARATOR OP_CODESEPARATOR]]
  */
case class ExecutionInProgressScriptProgram(
    txSignatureComponent: TxSigComponent,
    stack: List[ScriptToken],
    script: List[ScriptToken],
    originalScript: List[ScriptToken],
    altStack: List[ScriptToken],
    flags: Seq[ScriptFlag],
    lastCodeSeparator: Option[Int])
    extends StartedScriptProgram {
  override def failExecution(error: ScriptError): ExecutedScriptProgram = {
    ScriptProgram.toExecutedProgram(this).failExecution(error)
  }

  def replaceFlags(
      newFlags: Seq[ScriptFlag]): ExecutionInProgressScriptProgram = {
    this.copy(flags = newFlags)
  }

  /**
    * Removes the flags on the given [[org.bitcoins.core.script.ScriptProgram ScriptProgram]]
    * @return
    */
  def removeFlags(): ExecutionInProgressScriptProgram = {
    this.replaceFlags(Seq.empty)
  }

  def updateLastCodeSeparator(
      newLastCodeSeparator: Int): ExecutionInProgressScriptProgram = {
    this.copy(lastCodeSeparator = Some(newLastCodeSeparator))
  }
}

/**
  * Type for a [[org.bitcoins.core.script.ScriptProgram ScriptProgram]] that has been
  * evaluated completely by the
  * [[org.bitcoins.core.script.interpreter.ScriptInterpreter ScriptInterpreter]].
  *
  * @param error Indicates if the [[org.bitcoins.core.script.ScriptProgram ScriptProgram]] has
  *              encountered a [[org.bitcoins.core.script.result.ScriptError ScriptError]] in its execution.
  */
case class ExecutedScriptProgram(
    txSignatureComponent: TxSigComponent,
    stack: List[ScriptToken],
    script: List[ScriptToken],
    originalScript: List[ScriptToken],
    altStack: List[ScriptToken],
    flags: Seq[ScriptFlag],
    error: Option[ScriptError])
    extends StartedScriptProgram {
  override def failExecution(error: ScriptError): ExecutedScriptProgram = {
    this.copy(error = Some(error))
  }
}

/**
  * Factory companion object for [[org.bitcoins.core.script.ScriptProgram ScriptProgram]]
  */
object ScriptProgram extends BitcoinSLogger {

  //indicates whether the script or the stack needs to be updated
  sealed trait UpdateIndicator
  case object Stack extends UpdateIndicator
  case object Script extends UpdateIndicator
  case object AltStack extends UpdateIndicator
  case object OriginalScript extends UpdateIndicator

  def apply(
      oldProgram: ExecutionInProgressScriptProgram,
      flags: Seq[ScriptFlag]): ExecutionInProgressScriptProgram = {
    ExecutionInProgressScriptProgram(oldProgram.txSignatureComponent,
                                     oldProgram.stack,
                                     oldProgram.script,
                                     oldProgram.originalScript,
                                     oldProgram.altStack,
                                     flags,
                                     oldProgram.lastCodeSeparator)
  }

  def apply(
      oldProgram: ExecutionInProgressScriptProgram,
      tokens: Seq[ScriptToken],
      indicator: UpdateIndicator): ExecutionInProgressScriptProgram = {
    indicator match {
      case Stack =>
        ExecutionInProgressScriptProgram(
          oldProgram.txSignatureComponent,
          tokens.toList,
          oldProgram.script,
          oldProgram.originalScript,
          oldProgram.altStack,
          oldProgram.flags,
          oldProgram.lastCodeSeparator
        )
      case Script =>
        ExecutionInProgressScriptProgram(
          oldProgram.txSignatureComponent,
          oldProgram.stack,
          tokens.toList,
          oldProgram.originalScript,
          oldProgram.altStack,
          oldProgram.flags,
          oldProgram.lastCodeSeparator
        )
      case AltStack =>
        ExecutionInProgressScriptProgram(
          oldProgram.txSignatureComponent,
          oldProgram.stack,
          oldProgram.script,
          oldProgram.originalScript,
          tokens.toList,
          oldProgram.flags,
          oldProgram.lastCodeSeparator
        )
      case OriginalScript =>
        ExecutionInProgressScriptProgram(oldProgram.txSignatureComponent,
                                         oldProgram.stack,
                                         oldProgram.script,
                                         tokens.toList,
                                         oldProgram.altStack,
                                         oldProgram.flags,
                                         oldProgram.lastCodeSeparator)
    }
  }

  def apply(
      oldProgram: PreExecutionScriptProgram,
      stackTokens: Seq[ScriptToken],
      scriptTokens: Seq[ScriptToken]): PreExecutionScriptProgram = {
    val updatedStack = oldProgram.updateStack(stackTokens)
    val updatedScript = updatedStack.updateScript(scriptTokens)
    require(updatedStack.stack == stackTokens)
    require(updatedScript.script == scriptTokens)
    updatedScript
  }

  def apply(
      oldProgram: ExecutionInProgressScriptProgram,
      stackTokens: Seq[ScriptToken],
      scriptTokens: Seq[ScriptToken]): ExecutionInProgressScriptProgram = {
    val updatedStack = ScriptProgram(oldProgram, stackTokens, Stack)
    val updatedScript = ScriptProgram(updatedStack, scriptTokens, Script)
    require(updatedStack.stack == stackTokens)
    require(updatedScript.script == scriptTokens)
    updatedScript
  }

  /** Updates the [[org.bitcoins.core.script.constant.ScriptToken ScriptToken]]s
    * in either the stack or script and the last
    * [[org.bitcoins.core.script.crypto.OP_CODESEPARATOR OP_CODESEPARATOR]] index */
  def apply(
      oldProgram: ExecutionInProgressScriptProgram,
      tokens: Seq[ScriptToken],
      indicator: UpdateIndicator,
      lastCodeSeparator: Int): ExecutionInProgressScriptProgram = {
    val updatedIndicator = ScriptProgram(oldProgram, tokens, indicator)
    updatedIndicator.updateLastCodeSeparator(lastCodeSeparator)
  }

  /** Updates the [[org.bitcoins.core.script.ScriptProgram.Stack Stack]],
    * [[org.bitcoins.core.script.ScriptProgram.Script Script]],
    * [[org.bitcoins.core.script.ScriptProgram.AltStack AltStack]] of the given
    * [[org.bitcoins.core.script.ScriptProgram ScriptProgram]]. */
  def apply(
      oldProgram: PreExecutionScriptProgram,
      stack: Seq[ScriptToken],
      script: Seq[ScriptToken],
      altStack: Seq[ScriptToken]): PreExecutionScriptProgram = {
    val updatedProgramStack = oldProgram.updateStack(stack)
    val updatedProgramScript =
      updatedProgramStack.updateScript(script)
    val updatedProgramAltStack =
      updatedProgramScript.updateAltStack(altStack)
    updatedProgramAltStack
  }

  /** Updates the [[org.bitcoins.core.script.ScriptProgram.Stack Stack]],
    * [[org.bitcoins.core.script.ScriptProgram.Script Script]],
    * [[org.bitcoins.core.script.ScriptProgram.AltStack AltStack]] of the given
    * [[org.bitcoins.core.script.ScriptProgram ScriptProgram]]. */
  def apply(
      oldProgram: ExecutionInProgressScriptProgram,
      stack: Seq[ScriptToken],
      script: Seq[ScriptToken],
      altStack: Seq[ScriptToken]): ExecutionInProgressScriptProgram = {
    val updatedProgramStack = ScriptProgram(oldProgram, stack, Stack)
    val updatedProgramScript =
      ScriptProgram(updatedProgramStack, script, Script)
    val updatedProgramAltStack =
      ScriptProgram(updatedProgramScript, altStack, AltStack)
    updatedProgramAltStack
  }

  /** Changes a [[org.bitcoins.core.script.ScriptProgram ScriptProgram]] that is a
    * [[org.bitcoins.core.script.ExecutionInProgressScriptProgram ExecutionInProgressScriptProgram]]
    * and changes it to an [[org.bitcoins.core.script.ExecutedScriptProgram ExecutedScriptProgram]].*/
  def toExecutedProgram(
      executionInProgressScriptProgram: ExecutionInProgressScriptProgram): ExecutedScriptProgram = {
    ExecutedScriptProgram(
      executionInProgressScriptProgram.txSignatureComponent,
      executionInProgressScriptProgram.stack,
      executionInProgressScriptProgram.script,
      executionInProgressScriptProgram.originalScript,
      executionInProgressScriptProgram.altStack,
      executionInProgressScriptProgram.flags,
      None
    )
  }

  /** Changes a [[org.bitcoins.core.script.ScriptProgram ScriptProgram]] that is a
    * [[org.bitcoins.core.script.PreExecutionScriptProgram PreExecutionScriptProgram]] and changes it to an
    * [[org.bitcoins.core.script.ExecutionInProgressScriptProgram ExecutionInProgressScriptProgram]].*/
  def toExecutionInProgress(
      preExecutionScriptProgram: PreExecutionScriptProgram): ExecutionInProgressScriptProgram = {
    toExecutionInProgress(preExecutionScriptProgram, None)
  }

  /** Changes a [[org.bitcoins.core.script.ScriptProgram ScriptProgram]] that is a
    * [[org.bitcoins.core.script.PreExecutionScriptProgram PreExecutionScriptProgram]] and changes it to an
    * [[org.bitcoins.core.script.ExecutionInProgressScriptProgram ExecutionInProgressScriptProgram]]
    * given the stack state. */
  def toExecutionInProgress(
      preExecutionScriptProgram: PreExecutionScriptProgram,
      stack: Option[Seq[ScriptToken]]): ExecutionInProgressScriptProgram = {
    stack match {
      case Some(stackTokens) =>
        ExecutionInProgressScriptProgram(
          preExecutionScriptProgram.txSignatureComponent,
          stackTokens.toList,
          preExecutionScriptProgram.script,
          preExecutionScriptProgram.originalScript,
          preExecutionScriptProgram.altStack,
          preExecutionScriptProgram.flags,
          None
        )
      case None =>
        ExecutionInProgressScriptProgram(
          preExecutionScriptProgram.txSignatureComponent,
          preExecutionScriptProgram.stack,
          preExecutionScriptProgram.script,
          preExecutionScriptProgram.originalScript,
          preExecutionScriptProgram.altStack,
          preExecutionScriptProgram.flags,
          None
        )
    }
  }
}
