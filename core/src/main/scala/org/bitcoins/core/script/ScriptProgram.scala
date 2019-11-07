package org.bitcoins.core.script

import org.bitcoins.core.crypto._
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control.{OP_ELSE, OP_ENDIF, OP_IF, OP_NOTIF}
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.script.result._
import org.bitcoins.core.util.BitcoinScriptUtil

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

  def toExecutionInProgress: ExecutionInProgressScriptProgram = {
    ExecutionInProgressScriptProgram(
      txSignatureComponent,
      stack,
      script,
      originalScript,
      altStack,
      flags,
      None,
      Vector.empty
    )
  }

  override def failExecution(error: ScriptError): ExecutedScriptProgram = {
    this.toExecutionInProgress.failExecution(error)
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

  def updateStackAndScript(
      stackTokens: Seq[ScriptToken],
      scriptTokens: Seq[ScriptToken]): PreExecutionScriptProgram = {
    val updatedStack = this.updateStack(stackTokens)
    val updatedScript = updatedStack.updateScript(scriptTokens)
    require(updatedStack.stack == stackTokens)
    require(updatedScript.script == scriptTokens)
    updatedScript
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
  * @param conditions Every time OP_IF is encountered then the condition the operation takes as input is added to this Vector.
  *                   Every time OP_ELSE is encountered the last element is inverted.
  *                   Every time OP_ENDIF is encountered the last element is dropped.
  *                   The last element of this Vector represents whether operations should be executed.
  *                   The length of this Vector represents the current depth in the conditional tree.
  */
case class ExecutionInProgressScriptProgram(
    txSignatureComponent: TxSigComponent,
    stack: List[ScriptToken],
    script: List[ScriptToken],
    originalScript: List[ScriptToken],
    altStack: List[ScriptToken],
    flags: Seq[ScriptFlag],
    lastCodeSeparator: Option[Int],
    conditions: Vector[Boolean])
    extends StartedScriptProgram {

  def toExecutedProgram: ExecutedScriptProgram = {
    val errorOpt = if (conditions.nonEmpty) {
      Some(ScriptErrorUnbalancedConditional)
    } else {
      None
    }

    ExecutedScriptProgram(
      txSignatureComponent,
      stack,
      script,
      originalScript,
      altStack,
      flags,
      errorOpt
    )
  }

  override def failExecution(error: ScriptError): ExecutedScriptProgram = {
    this.toExecutedProgram.failExecution(error)
  }

  def replaceFlags(
      newFlags: Seq[ScriptFlag]): ExecutionInProgressScriptProgram = {
    this.copy(flags = newFlags)
  }

  def isInExecutionBranch: Boolean = {
    conditions.forall(_ == true)
  }

  def shouldExecuteNextOperation: Boolean = {
    script.headOption match {
      case None                                        => false
      case Some(OP_IF | OP_NOTIF | OP_ELSE | OP_ENDIF) => true
      case Some(_)                                     => isInExecutionBranch
    }
  }

  def addCondition(condition: Boolean): ExecutionInProgressScriptProgram = {
    this.copy(conditions = conditions :+ condition)
  }

  def invertCondition(): StartedScriptProgram = {
    if (conditions.isEmpty) {
      this.failExecution(ScriptErrorUnbalancedConditional)
    } else {
      this.copy(conditions =
        conditions.updated(conditions.length - 1, !conditions.last))
    }
  }

  def removeCondition(): StartedScriptProgram = {
    if (conditions.nonEmpty) {
      this.copy(conditions = conditions.dropRight(1))
    } else {
      this.failExecution(ScriptErrorUnbalancedConditional)
    }
  }

  /**
    * Removes the flags on the given [[org.bitcoins.core.script.ScriptProgram ScriptProgram]]
    *
    * @return
    */
  def removeFlags(): ExecutionInProgressScriptProgram = {
    this.replaceFlags(Seq.empty)
  }

  def updateStack(
      tokens: Seq[ScriptToken]): ExecutionInProgressScriptProgram = {
    this.copy(stack = tokens.toList)
  }

  def updateAltStack(
      tokens: Seq[ScriptToken]): ExecutionInProgressScriptProgram = {
    this.copy(altStack = tokens.toList)
  }

  def updateScript(
      tokens: Seq[ScriptToken]): ExecutionInProgressScriptProgram = {
    this.copy(script = tokens.toList)
  }

  def updateStackAndScript(
      stack: Seq[ScriptToken],
      script: Seq[ScriptToken]): ExecutionInProgressScriptProgram = {
    this
      .updateStack(stack)
      .updateScript(script)
  }

  def updateOriginalScript(
      tokens: Seq[ScriptToken]): ExecutionInProgressScriptProgram = {
    this.copy(originalScript = tokens.toList)
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
