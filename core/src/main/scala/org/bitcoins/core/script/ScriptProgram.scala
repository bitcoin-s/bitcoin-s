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
      txSignatureComponent = txSignatureComponent,
      stack = stack,
      script = script,
      originalScript = originalScript,
      altStack = altStack,
      flags = flags,
      lastCodeSeparator = None,
      trueCount = 0,
      falseAndIgnoreCount = 0
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
  * @param trueCount The depth of OP_IFs/OP_NOTIFs we've entered on the true condition before the first false.
  * @param falseAndIgnoreCount The depth of OP_IFs/OP_NOTIFs we've entered after and including the first false condition.
  *                            Every OP_IF/OP_NOTIF adds to trueCount or falseAndIgnoreCount.
  *                            OP_ELSE has an effect only when falseAndIgnoreCount == 0 or 1, in which case it moves
  *                            1 from trueCount to falseAndIgnoreCount or vice versa.
  *                            OP_ENDIF subtracts one from either falseAndIgnoreCount or trueCount if falseAndIgnoreCount == 0.
  *                            trueCount + falseAndIgnoreCount represents the current depth in the conditional tree.
  *                            falseAndIgnoreCount == 0 represents whether operations should be executed.
  */
case class ExecutionInProgressScriptProgram(
    txSignatureComponent: TxSigComponent,
    stack: List[ScriptToken],
    script: List[ScriptToken],
    originalScript: List[ScriptToken],
    altStack: List[ScriptToken],
    flags: Seq[ScriptFlag],
    lastCodeSeparator: Option[Int],
    trueCount: Int,
    falseAndIgnoreCount: Int)
    extends StartedScriptProgram {

  def toExecutedProgram: ExecutedScriptProgram = {
    val errorOpt = if (trueCount + falseAndIgnoreCount > 0) {
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

  /** Non-conditional opcodes should be executed only if this is true */
  def isInExecutionBranch: Boolean = {
    falseAndIgnoreCount == 0
  }

  /** ScriptInterpreter should look at the script head only if this is true.
    *
    * Note that OP_IF, OP_NOTIF, OP_ELSE, and OP_ENDIF must be executed even if
    * isInExecutionBranch is false as they must modify the states of trueCount and falseAndIgnoreCount.
    */
  def shouldExecuteNextOperation: Boolean = {
    script.headOption match {
      case None                                        => false
      case Some(OP_IF | OP_NOTIF | OP_ELSE | OP_ENDIF) => true
      case Some(_)                                     => isInExecutionBranch
    }
  }

  /** Should be called for every OP_IF and OP_NOTIF with whether the first (true)
    * or second (false) branch should be taken.
    */
  def addCondition(condition: Boolean): ExecutionInProgressScriptProgram = {
    if (!isInExecutionBranch || !condition) {
      this.copy(falseAndIgnoreCount = falseAndIgnoreCount + 1)
    } else {
      this.copy(trueCount = trueCount + 1)
    }
  }

  /** Should be called on for every OP_ELSE */
  def invertCondition(): StartedScriptProgram = {
    if (trueCount + falseAndIgnoreCount == 0) {
      this.failExecution(ScriptErrorUnbalancedConditional)
    } else {
      if (falseAndIgnoreCount > 1) {
        // Do nothing, we aren't in an execution now branch anyway
        this
      } else if (falseAndIgnoreCount == 1) {
        this.copy(trueCount = trueCount + 1, falseAndIgnoreCount = 0)
      } else { // Case falseAndIgnoreCount = 0, trueCount > 0
        this.copy(trueCount = trueCount - 1, falseAndIgnoreCount = 1)
      }
    }
  }

  /** Should be called on for every OP_ENDIF */
  def removeCondition(): StartedScriptProgram = {
    if (trueCount + falseAndIgnoreCount > 0) {
      if (falseAndIgnoreCount > 0) {
        this.copy(falseAndIgnoreCount = falseAndIgnoreCount - 1)
      } else {
        this.copy(trueCount = trueCount - 1)
      }
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
