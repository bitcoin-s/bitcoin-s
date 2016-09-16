package org.bitcoins.core.script


import org.bitcoins.core.crypto.TransactionSignatureComponent
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.script.result._

/**
  * Created by chris on 2/3/16.
 */
sealed trait ScriptProgram {


  /**
   * This contains all relevant information for hashing and checking a [[org.bitcoins.core.protocol.script.ScriptSignature]] for a [[Transaction]].
    *
    * @return
   */
  def txSignatureComponent : TransactionSignatureComponent

  /**
   * The current state of the stack for execution of the [[ScriptProgram]].
    *
    * @return
   */
  def stack : List[ScriptToken]

  /**
   * The script operations that need to still be executed.
    *
    * @return
   */
  def script : List[ScriptToken]


  /**
   * The original script that was given.
    *
    * @return
   */
  def originalScript : List[ScriptToken]

  /**
   * The alternative stack is used in some Script op codes.
    *
    * @return
   */
  def altStack : List[ScriptToken]

  /**
   * [[ScriptFlag]] that are run with the script.
   * These flags indicate special conditions that a script needs to be run with.
   * https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.h#L31
 *
   * @return
   */
  def flags : Seq[ScriptFlag]

  /**
    * Returns true if the stack top is true
    * @return
    */
  def stackTopIsTrue = !stackTopIsFalse


  /**
    * Returns true if the stack top is false
    * @return
    */
  def stackTopIsFalse : Boolean = {
    if (stack.headOption.isDefined &&
      (stack.head.hex == OP_FALSE.hex || stack.head.hex == ScriptNumber.negativeZero.hex ||
        stack.head.hex == ScriptNumber.zero.hex)) true
    else if (!stack.headOption.isDefined) true
    else false
  }


}


/**
 * This represents a [[ScriptProgram]] before any script operations have been executed in the [[org.bitcoins.core.script.interpreter.ScriptInterpreter]].
 */
sealed trait PreExecutionScriptProgram extends ScriptProgram
sealed trait ExecutionInProgressScriptProgram extends ScriptProgram {
  /**
   * The index of the last [[org.bitcoins.core.script.crypto.OP_CODESEPARATOR]]
   * @return
   */
  def lastCodeSeparator : Option[Int]

}

sealed trait ExecutedScriptProgram extends ScriptProgram {
  /**
   * Indicates if the [[ScriptProgram]] has encountered a [[ScriptError]] in its execution.
   * @return
   */
  def error : Option[ScriptError]
}

/**
 * Factory companion object for [[ScriptProgram]]
 */
object ScriptProgram {
  /**
   * Implementation type for a [[PreExecutionScriptProgram]] - a [[ScriptProgram]] that has not yet begun being
    * evaluated  by the [[org.bitcoins.core.script.interpreter.ScriptInterpreter]].
   */
  private sealed case class PreExecutionScriptProgramImpl(txSignatureComponent : TransactionSignatureComponent,
    stack : List[ScriptToken],script : List[ScriptToken], originalScript : List[ScriptToken], altStack : List[ScriptToken],
    flags : Seq[ScriptFlag]) extends PreExecutionScriptProgram

  /**
   * Implementation type for a [[ExecutionInProgressScriptProgram]] - a [[ScriptProgram]] that is currently being
    * evaluated by the [[org.bitcoins.core.script.interpreter.ScriptInterpreter]].
   */
  private sealed case class ExecutionInProgressScriptProgramImpl(txSignatureComponent : TransactionSignatureComponent,
    stack : List[ScriptToken],script : List[ScriptToken], originalScript : List[ScriptToken], altStack : List[ScriptToken],
    flags : Seq[ScriptFlag], lastCodeSeparator : Option[Int]) extends ExecutionInProgressScriptProgram

  /**
   * The implementation type for a [[ExecutedScriptProgram]] - a [[ScriptProgram]] that has been evaluated completely
    * by the [[org.bitcoins.core.script.interpreter.ScriptInterpreter]].
   */
  private sealed case class ExecutedScriptProgramImpl(txSignatureComponent : TransactionSignatureComponent,
    stack : List[ScriptToken],script : List[ScriptToken], originalScript : List[ScriptToken], altStack : List[ScriptToken],
    flags : Seq[ScriptFlag],  error : Option[ScriptError]) extends ExecutedScriptProgram


  //indicates whether the script or the stack needs to be updated
  sealed trait UpdateIndicator
  case object Stack extends UpdateIndicator
  case object Script extends UpdateIndicator
  case object AltStack extends UpdateIndicator
  case object OriginalScript extends UpdateIndicator


  /**
   * Sets a [[ScriptError]] on a given [[ScriptProgram]].
   * @param oldProgram the program who has hit an invalid state
   * @param error the error that the program hit while being executed in the script interpreter
   * @return the ExecutedScriptProgram with the given error set inside of the trait
   */
  def factory(oldProgram : ScriptProgram, error : ScriptError) : ExecutedScriptProgram = oldProgram match {
    case program : PreExecutionScriptProgram =>
      throw new RuntimeException("We cannot set an error on the script program before it is executed")
    case program : ExecutionInProgressScriptProgram =>
      ExecutedScriptProgramImpl(program.txSignatureComponent, program.stack, program.script,program.originalScript,
        program.altStack,  program.flags, Some(error))
    case program : ExecutedScriptProgram =>
      ExecutedScriptProgramImpl(program.txSignatureComponent, program.stack, program.script,program.originalScript,
        program.altStack,  program.flags, Some(error))
  }


  /**
    * Updates the [[ScriptFlag]] on a given [[ScriptProgram]].
    * @return
    */
  def factory(oldProgram : ScriptProgram, flags : Seq[ScriptFlag]) : ScriptProgram = oldProgram match {
    case program : PreExecutionScriptProgram =>
      PreExecutionScriptProgramImpl(program.txSignatureComponent,program.stack,program.script,program.originalScript,
      program.altStack,flags)
    case program : ExecutionInProgressScriptProgram =>
      ExecutionInProgressScriptProgramImpl(program.txSignatureComponent, program.stack,program.script,program.originalScript,
        program.altStack, flags, program.lastCodeSeparator)
    case program : ExecutedScriptProgram =>
      throw new RuntimeException("Cannot update the script flags on a program that has been executed")
  }


  /**
    * Updates the [[ScriptProgram]] with a sequence of [[ScriptToken]]s depending on the [[UpdateIndicator]].
    * @return
    */
  def factory(oldProgram : ScriptProgram, tokens : Seq[ScriptToken], indicator : UpdateIndicator) : ScriptProgram = {
    indicator match {
      case Stack =>
        oldProgram match {
          case program : PreExecutionScriptProgram =>
            PreExecutionScriptProgramImpl(program.txSignatureComponent, tokens.toList,program.script,program.originalScript,
              program.altStack,program.flags)

          case program : ExecutionInProgressScriptProgram =>
            ExecutionInProgressScriptProgramImpl(program.txSignatureComponent,tokens.toList,program.script,program.originalScript,
              program.altStack,program.flags,program.lastCodeSeparator)

          case program : ExecutedScriptProgram =>
            throw new RuntimeException("Cannot update stack for program that has been fully executed")
        }
      case Script =>
        oldProgram match {
          case program : PreExecutionScriptProgram =>
            PreExecutionScriptProgramImpl(program.txSignatureComponent, program.stack,tokens.toList,program.originalScript,
              program.altStack,program.flags)
          case program : ExecutionInProgressScriptProgram =>
            ExecutionInProgressScriptProgramImpl(program.txSignatureComponent, program.stack, tokens.toList, program.originalScript,
              program.altStack, program.flags,program.lastCodeSeparator)
          case program : ExecutedScriptProgram =>
            throw new RuntimeException("Cannot update the script for a program that has been fully executed")
        }
      case AltStack => oldProgram match {
        case program : PreExecutionScriptProgram =>
          PreExecutionScriptProgramImpl(program.txSignatureComponent, program.stack,program.script,program.originalScript,
            tokens.toList,program.flags)
        case program : ExecutionInProgressScriptProgram =>
          ExecutionInProgressScriptProgramImpl(program.txSignatureComponent, program.stack, program.script, program.originalScript,
            tokens.toList, program.flags, program.lastCodeSeparator)
        case program : ExecutedScriptProgram =>
          throw new RuntimeException("Cannot update the alt stack for a program that has been fully executed")
      }

      case OriginalScript => oldProgram match {
        case program : PreExecutionScriptProgram =>
          PreExecutionScriptProgramImpl(program.txSignatureComponent, program.stack,program.script,tokens.toList,
            program.altStack,program.flags)
        case program : ExecutionInProgressScriptProgram =>
          ExecutionInProgressScriptProgramImpl(program.txSignatureComponent, program.stack, program.script, tokens.toList,
            program.altStack, program.flags, program.lastCodeSeparator)
        case program : ExecutedScriptProgram =>
          throw new RuntimeException("Cannot update the alt stack for a program that has been fully executed")
      }

    }
  }

  /**
    * Changes the [[ScriptToken]] assembly-construction of the script or stack inside a [[ScriptProgram]].
    * @return
    */
  def factory(oldProgram : ScriptProgram, stackTokens : Seq[ScriptToken], scriptTokens : Seq[ScriptToken]) : ScriptProgram = {
    val updatedStack = apply(oldProgram,stackTokens,Stack)
    val updatedScript = apply(updatedStack,scriptTokens,Script)
    updatedScript
  }


  /**
    * Updates the last [[org.bitcoins.core.script.crypto.OP_CODESEPARATOR]] index.
    * @return
    */
  def factory(oldProgram : ExecutionInProgressScriptProgram, lastCodeSeparator : Int) : ExecutionInProgressScriptProgram = {
    ExecutionInProgressScriptProgramImpl(oldProgram.txSignatureComponent,
      oldProgram.stack, oldProgram.script, oldProgram.originalScript,
      oldProgram.altStack, oldProgram.flags,Some(lastCodeSeparator))
  }

  /**
    * Updates the [[ScriptToken]]s in either the stack or script and the last [[org.bitcoins.core.script.crypto.OP_CODESEPARATOR]] index
    * @return
    */
  def factory(oldProgram : ExecutionInProgressScriptProgram, tokens : Seq[ScriptToken], indicator: UpdateIndicator,
              lastCodeSeparator : Int) : ExecutionInProgressScriptProgram = {
    val updatedIndicator = apply(oldProgram, tokens, indicator)
    updatedIndicator match {
      case e : ExecutionInProgressScriptProgram =>
        apply(e,lastCodeSeparator)
      case _ : PreExecutionScriptProgram | _ : ExecutedScriptProgram =>
        throw new RuntimeException("We must have a ExecutionInProgressScriptProgram to update the last OP_CODESEPARATOR index")
    }
  }

  /**
    * Updates the [[Stack]], [[Script]], [[AltStack]] of the given [[ScriptProgram]].
    * @return
    */
  def factory(oldProgram : ScriptProgram, stack : Seq[ScriptToken], script : Seq[ScriptToken], altStack : Seq[ScriptToken]) : ScriptProgram = {
    val updatedProgramStack = apply(oldProgram,stack, Stack)
    val updatedProgramScript = apply(updatedProgramStack, script, Script)
    val updatedProgramAltStack = apply(updatedProgramScript, altStack, AltStack)
    updatedProgramAltStack
  }

  /**
    * Creates a new [[ScriptProgram]] that can be used to verify if a [[Transaction]] at the given inputIndex
    * spends a given [[ScriptPubKey]] correctly. Assumes that the [[Script]] to be executed is the ???
    * @param transaction the transaction that is being checked
    * @param scriptPubKey the scriptPubKey for which the input is spending
    * @param inputIndex the input's index inside of transaction which we are spending
    * @param flags the flags which we are enforcing inside of the script interpreter
    * @return the script program representing all of this information
    */
  def factory(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : UInt32,
              flags : Seq[ScriptFlag]) : PreExecutionScriptProgram = {
    val script = transaction.inputs(inputIndex.toInt).scriptSignature.asm
    apply(transaction,scriptPubKey,inputIndex,script.toList,flags)
  }

  /**
    * Creates a new [[ScriptProgram]] that can be used to verify if a [[Transaction]] at the given inputIndex
    * spends a given [[ScriptPubKey]] correctly.
    *
    * @param transaction the transaction that is being checked
    * @param scriptPubKey the scriptPubKey for which the input is spending
    * @param inputIndex the input's index inside of transaction which we are spending
    * @param script the script that we are currently executing
    * @param flags the flags which we are enforcing inside of the script interpreter
    * @return the script program representing all of this information
    */
  def factory(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : UInt32, script : Seq[ScriptToken],
              flags : Seq[ScriptFlag]) : PreExecutionScriptProgram = {
    val txSignatureComponent = TransactionSignatureComponent(transaction,inputIndex,scriptPubKey,flags)
    PreExecutionScriptProgramImpl(txSignatureComponent,List(),script.toList,script.toList,List(),flags)
  }


  /**
    * The intention for this factory function is to allow us to create a [[ScriptProgram]] that already has a stack state. This
    * is useful for after execution of a scriptSig, copying the stack into this program with the [[ScriptPubKey]] to
    * run inside the script.
    * @param transaction the transaction being checked
    * @param scriptPubKey the scriptPubKey which the input is spending
    * @param inputIndex the input's index inside of the transaction we are spending
    * @param stack the current stack state of the program
    * @param script the script that we need to execute
    * @param flags the flags which we are enforcing inside of the script interpreter
    */
  def factory(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : UInt32, stack : Seq[ScriptToken],
              script : Seq[ScriptToken], flags : Seq[ScriptFlag]) : ScriptProgram = {
    val program = factory(transaction,scriptPubKey,inputIndex,script,flags)
    apply(program,stack,Stack)
  }


  /**
    * The intention for this factory function is to allow us to create a [[ScriptProgram]] that already has a stack state. This
    * is useful for after execution of a scriptSig, copying the stack into this program with the scriptPubKey read to
    * run inside the script variable
    * @param txSignatureComponent the relevant transaction information for execution of a script program
    * @param stack the current stack state of the program
    * @param script the script that we need to execute
    * @return
    */
  def factory(txSignatureComponent : TransactionSignatureComponent, stack : Seq[ScriptToken], script : Seq[ScriptToken]) : ScriptProgram = {
    apply(txSignatureComponent.transaction,txSignatureComponent.scriptPubKey,txSignatureComponent.inputIndex,
      stack,script,txSignatureComponent.flags)
  }

  def apply(oldProgram : ScriptProgram, error : ScriptError) : ExecutedScriptProgram = factory(oldProgram, error)

  def apply(oldProgram : ScriptProgram, flags : Seq[ScriptFlag]) : ScriptProgram = factory(oldProgram, flags)

  def apply(oldProgram : ScriptProgram, tokens : Seq[ScriptToken], indicator : UpdateIndicator) : ScriptProgram =
    factory(oldProgram, tokens, indicator)

  def apply(oldProgram : ScriptProgram, stackTokens : Seq[ScriptToken], scriptTokens : Seq[ScriptToken]) : ScriptProgram =
    factory(oldProgram, stackTokens, scriptTokens)

  def apply(oldProgram : ExecutionInProgressScriptProgram, lastCodeSeparator : Int) : ExecutionInProgressScriptProgram = factory(oldProgram, lastCodeSeparator)

  def apply(oldProgram : ExecutionInProgressScriptProgram, tokens : Seq[ScriptToken], indicator: UpdateIndicator, lastCodeSeparator : Int) : ExecutionInProgressScriptProgram =
    factory(oldProgram, tokens, indicator, lastCodeSeparator)

  def apply(oldProgram : ScriptProgram, stack : Seq[ScriptToken], script : Seq[ScriptToken], altStack : Seq[ScriptToken],
            updateIndicator: UpdateIndicator) : ScriptProgram = factory(oldProgram, stack, script, altStack)

  def apply(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : UInt32,
            flags : Seq[ScriptFlag]) : PreExecutionScriptProgram = factory(transaction, scriptPubKey, inputIndex, flags)

  def apply(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : UInt32, script : Seq[ScriptToken],
            flags : Seq[ScriptFlag]) : PreExecutionScriptProgram = factory(transaction, scriptPubKey, inputIndex, script, flags)

  def apply(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : UInt32, stack : Seq[ScriptToken],
            script : Seq[ScriptToken], flags : Seq[ScriptFlag]) : ScriptProgram = factory(transaction, scriptPubKey, inputIndex, stack, script, flags)

  def apply(txSignatureComponent : TransactionSignatureComponent, stack : Seq[ScriptToken], script : Seq[ScriptToken]) : ScriptProgram =
    factory(txSignatureComponent, stack, script)


  def apply(txSignatureComponent: TransactionSignatureComponent): PreExecutionScriptProgram = {
    ScriptProgram(txSignatureComponent.transaction, txSignatureComponent.scriptPubKey, txSignatureComponent.inputIndex,txSignatureComponent.flags)
  }

  /**
   * Changes a [[ScriptProgram]] that is a [[ExecutionInProgressScriptProgram]] and changes it to an [[ExecutedScriptProgram]].
   * @param executionInProgressScriptProgram
   * @return
   */
  def toExecutedProgram(executionInProgressScriptProgram: ExecutionInProgressScriptProgram) : ExecutedScriptProgram = {
    ExecutedScriptProgramImpl(executionInProgressScriptProgram.txSignatureComponent, executionInProgressScriptProgram.stack,
      executionInProgressScriptProgram.script,executionInProgressScriptProgram.originalScript,executionInProgressScriptProgram.altStack,
      executionInProgressScriptProgram.flags,None)
  }

  /**
   * Changes a [[ScriptProgram]] that is a [[PreExecutionScriptProgram]] and changes it to an [[ExecutionInProgressScriptProgram]].
   * @param preExecutionScriptProgram
   * @return
   */
  def toExecutionInProgress(preExecutionScriptProgram: PreExecutionScriptProgram) : ExecutionInProgressScriptProgram = {
    toExecutionInProgress(preExecutionScriptProgram,None)
  }

  /**
   * Changes a [[ScriptProgram]] that is a [[PreExecutionScriptProgram]] and changes it to an [[ExecutionInProgressScriptProgram]] given the stack state.
   * @param preExecutionScriptProgram
   * @param stack
   * @return
   */
  def toExecutionInProgress(preExecutionScriptProgram: PreExecutionScriptProgram, stack : Option[List[ScriptToken]]) : ExecutionInProgressScriptProgram = {
    stack match {
      case Some(stackTokens) => ExecutionInProgressScriptProgramImpl(preExecutionScriptProgram.txSignatureComponent,stackTokens,preExecutionScriptProgram.script,
    preExecutionScriptProgram.originalScript,preExecutionScriptProgram.altStack,preExecutionScriptProgram.flags, None)
      case None =>
        ExecutionInProgressScriptProgramImpl(preExecutionScriptProgram.txSignatureComponent,preExecutionScriptProgram.stack,preExecutionScriptProgram.script,
          preExecutionScriptProgram.originalScript,preExecutionScriptProgram.altStack,preExecutionScriptProgram.flags, None)
    }
  }




}

