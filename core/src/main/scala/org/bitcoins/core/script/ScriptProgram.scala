package org.bitcoins.core.script

import org.bitcoins.core.crypto._
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.script.result._
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}

import scala.annotation.tailrec

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
    extends ScriptProgram

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
    extends ScriptProgram

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
    extends ScriptProgram

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

  /**
    * Sets a [[org.bitcoins.core.script.result.ScriptError ScriptError]] on a given
    * [[org.bitcoins.core.script.ScriptProgram ScriptProgram]].
    * @param oldProgram the program who has hit an invalid state
    * @param error the error that the program hit while being executed in the script interpreter
    * @return the ExecutedScriptProgram with the given error set inside of the trait
    */
  @tailrec
  def apply(
      oldProgram: ScriptProgram,
      error: ScriptError): ExecutedScriptProgram = oldProgram match {
    case program: PreExecutionScriptProgram =>
      ScriptProgram(ScriptProgram.toExecutionInProgress(program), error)
    case program: ExecutionInProgressScriptProgram =>
      ExecutedScriptProgram(program.txSignatureComponent,
                            program.stack,
                            program.script,
                            program.originalScript,
                            program.altStack,
                            program.flags,
                            Some(error))
    case program: ExecutedScriptProgram =>
      ExecutedScriptProgram(program.txSignatureComponent,
                            program.stack,
                            program.script,
                            program.originalScript,
                            program.altStack,
                            program.flags,
                            Some(error))
  }

  def apply(oldProgram: ScriptProgram, flags: Seq[ScriptFlag]): ScriptProgram =
    oldProgram match {
      case program: PreExecutionScriptProgram =>
        PreExecutionScriptProgram(program.txSignatureComponent,
                                  program.stack,
                                  program.script,
                                  program.originalScript,
                                  program.altStack,
                                  flags)
      case program: ExecutionInProgressScriptProgram =>
        ExecutionInProgressScriptProgram(program.txSignatureComponent,
                                         program.stack,
                                         program.script,
                                         program.originalScript,
                                         program.altStack,
                                         flags,
                                         program.lastCodeSeparator)
      case _: ExecutedScriptProgram =>
        throw new RuntimeException(
          "Cannot update the script flags on a program that has been executed")
    }

  def apply(
      oldProgram: ScriptProgram,
      tokens: Seq[ScriptToken],
      indicator: UpdateIndicator): ScriptProgram = {
    indicator match {
      case Stack =>
        oldProgram match {
          case program: PreExecutionScriptProgram =>
            PreExecutionScriptProgram(program.txSignatureComponent,
                                      tokens.toList,
                                      program.script,
                                      program.originalScript,
                                      program.altStack,
                                      program.flags)
          case program: ExecutionInProgressScriptProgram =>
            ExecutionInProgressScriptProgram(program.txSignatureComponent,
                                             tokens.toList,
                                             program.script,
                                             program.originalScript,
                                             program.altStack,
                                             program.flags,
                                             program.lastCodeSeparator)
          case _: ExecutedScriptProgram =>
            throw new RuntimeException(
              "Cannot update stack for program that has been fully executed")
        }
      case Script =>
        oldProgram match {
          case program: PreExecutionScriptProgram =>
            PreExecutionScriptProgram(program.txSignatureComponent,
                                      program.stack,
                                      tokens.toList,
                                      program.originalScript,
                                      program.altStack,
                                      program.flags)
          case program: ExecutionInProgressScriptProgram =>
            ExecutionInProgressScriptProgram(program.txSignatureComponent,
                                             program.stack,
                                             tokens.toList,
                                             program.originalScript,
                                             program.altStack,
                                             program.flags,
                                             program.lastCodeSeparator)
          case _: ExecutedScriptProgram =>
            throw new RuntimeException(
              "Cannot update the script for a program that has been fully executed")
        }
      case AltStack =>
        oldProgram match {
          case program: PreExecutionScriptProgram =>
            PreExecutionScriptProgram(program.txSignatureComponent,
                                      program.stack,
                                      program.script,
                                      program.originalScript,
                                      tokens.toList,
                                      program.flags)
          case program: ExecutionInProgressScriptProgram =>
            ExecutionInProgressScriptProgram(program.txSignatureComponent,
                                             program.stack,
                                             program.script,
                                             program.originalScript,
                                             tokens.toList,
                                             program.flags,
                                             program.lastCodeSeparator)
          case _: ExecutedScriptProgram =>
            throw new RuntimeException(
              "Cannot update the alt stack for a program that has been fully executed")
        }

      case OriginalScript =>
        oldProgram match {
          case program: PreExecutionScriptProgram =>
            PreExecutionScriptProgram(program.txSignatureComponent,
                                      program.stack,
                                      program.script,
                                      tokens.toList,
                                      program.altStack,
                                      program.flags)
          case program: ExecutionInProgressScriptProgram =>
            ExecutionInProgressScriptProgram(program.txSignatureComponent,
                                             program.stack,
                                             program.script,
                                             tokens.toList,
                                             program.altStack,
                                             program.flags,
                                             program.lastCodeSeparator)
          case _: ExecutedScriptProgram =>
            throw new RuntimeException(
              "Cannot update the original script for a program that has been fully executed")
        }

    }
  }

  def apply(
      oldProgram: ScriptProgram,
      stackTokens: Seq[ScriptToken],
      scriptTokens: Seq[ScriptToken]): ScriptProgram = {
    val updatedStack = ScriptProgram(oldProgram, stackTokens, Stack)
    val updatedScript = ScriptProgram(updatedStack, scriptTokens, Script)
    require(updatedStack.stack == stackTokens)
    require(updatedScript.script == scriptTokens)
    updatedScript
  }

  /** Updates the last [[org.bitcoins.core.script.crypto.OP_CODESEPARATOR OP_CODESEPARATOR]] index. */
  def apply(
      oldProgram: ExecutionInProgressScriptProgram,
      lastCodeSeparator: Int): ExecutionInProgressScriptProgram = {
    ExecutionInProgressScriptProgram(
      oldProgram.txSignatureComponent,
      oldProgram.stack,
      oldProgram.script,
      oldProgram.originalScript,
      oldProgram.altStack,
      oldProgram.flags,
      Some(lastCodeSeparator)
    )
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
    updatedIndicator match {
      case e: ExecutionInProgressScriptProgram =>
        ScriptProgram(e, lastCodeSeparator)
      case _: PreExecutionScriptProgram | _: ExecutedScriptProgram =>
        throw new RuntimeException(
          "We must have a ExecutionInProgressScriptProgram to update the last OP_CODESEPARATOR index")
    }
  }

  /** Updates the [[org.bitcoins.core.script.ScriptProgram.Stack Stack]],
    * [[org.bitcoins.core.script.ScriptProgram.Script Script]],
    * [[org.bitcoins.core.script.ScriptProgram.AltStack AltStack]] of the given
    * [[org.bitcoins.core.script.ScriptProgram ScriptProgram]]. */
  def apply(
      oldProgram: ScriptProgram,
      stack: Seq[ScriptToken],
      script: Seq[ScriptToken],
      altStack: Seq[ScriptToken]): ScriptProgram = {
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
