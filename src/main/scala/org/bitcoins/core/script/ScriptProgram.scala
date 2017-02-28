package org.bitcoins.core.script


import org.bitcoins.core.crypto.{BaseTransactionSignatureComponent, TransactionSignatureComponent, WitnessV0TransactionSignatureComponent}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{BaseTransaction, Transaction, WitnessTransaction}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.script.result._
import org.bitcoins.core.util.BitcoinScriptUtil

/**
  * Created by chris on 2/3/16.
 */
sealed trait ScriptProgram {


  /**
   * This contains all relevant information for hashing and checking a [[org.bitcoins.core.protocol.script.ScriptSignature]] for a [[Transaction]].
   */
  def txSignatureComponent : TransactionSignatureComponent

  /** The current state of the stack for execution of the [[ScriptProgram]]. */
  def stack : List[ScriptToken]

  /** The script operations that need to still be executed. */
  def script : List[ScriptToken]

  /** The original script that was given. */
  def originalScript : List[ScriptToken]

  /** The alternative stack is used in some Script op codes. */
  def altStack : List[ScriptToken]

  /**
   * [[ScriptFlag]] that are run with the script.
   * These flags indicate special conditions that a script needs to be run with.
   * [[https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.h#L31]]
   * @return
   */
  def flags : Seq[ScriptFlag]

  /** Returns true if the stack top is true */
  def stackTopIsTrue = stack.headOption.isDefined && BitcoinScriptUtil.castToBool(stack.head)

  /** Returns true if the stack top is false */
  def stackTopIsFalse : Boolean = !stackTopIsTrue

}


/**
 * This represents a [[ScriptProgram]] before any script operations have been executed in the [[org.bitcoins.core.script.interpreter.ScriptInterpreter]].
 */
sealed trait PreExecutionScriptProgram extends ScriptProgram
sealed trait ExecutionInProgressScriptProgram extends ScriptProgram {

  /** The index of the last [[org.bitcoins.core.script.crypto.OP_CODESEPARATOR]] */
  def lastCodeSeparator : Option[Int]

}

sealed trait ExecutedScriptProgram extends ScriptProgram {
  /** Indicates if the [[ScriptProgram]] has encountered a [[ScriptError]] in its execution.*/
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
  def apply(oldProgram : ScriptProgram, error : ScriptError) : ExecutedScriptProgram = oldProgram match {
    case program : PreExecutionScriptProgram =>
      ScriptProgram(ScriptProgram.toExecutionInProgress(program),error)
    case program : ExecutionInProgressScriptProgram =>
      ExecutedScriptProgramImpl(program.txSignatureComponent, program.stack, program.script,program.originalScript,
        program.altStack,  program.flags, Some(error))
    case program : ExecutedScriptProgram =>
      ExecutedScriptProgramImpl(program.txSignatureComponent, program.stack, program.script,program.originalScript,
        program.altStack,  program.flags, Some(error))
  }

  def apply(oldProgram : ScriptProgram, flags : Seq[ScriptFlag]) : ScriptProgram = oldProgram match {
    case program : PreExecutionScriptProgram =>
      PreExecutionScriptProgramImpl(program.txSignatureComponent,program.stack,program.script,program.originalScript,
        program.altStack,flags)
    case program : ExecutionInProgressScriptProgram =>
      ExecutionInProgressScriptProgramImpl(program.txSignatureComponent, program.stack,program.script,program.originalScript,
        program.altStack, flags, program.lastCodeSeparator)
    case program : ExecutedScriptProgram =>
      throw new RuntimeException("Cannot update the script flags on a program that has been executed")
  }

  def apply(oldProgram : ScriptProgram, tokens : Seq[ScriptToken], indicator : UpdateIndicator) : ScriptProgram = {
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
          throw new RuntimeException("Cannot update the original script for a program that has been fully executed")
      }

    }
  }

  def apply(oldProgram : ScriptProgram, stackTokens : Seq[ScriptToken], scriptTokens : Seq[ScriptToken]) : ScriptProgram = {
    val updatedStack = ScriptProgram(oldProgram,stackTokens,Stack)
    val updatedScript = ScriptProgram(updatedStack,scriptTokens,Script)
    require(updatedStack.stack == stackTokens)
    require(updatedScript.script == scriptTokens)
    updatedScript
  }
  /** Updates the last [[org.bitcoins.core.script.crypto.OP_CODESEPARATOR]] index. */
  def apply(oldProgram : ExecutionInProgressScriptProgram, lastCodeSeparator : Int) : ExecutionInProgressScriptProgram = {
    ExecutionInProgressScriptProgramImpl(oldProgram.txSignatureComponent,
      oldProgram.stack, oldProgram.script, oldProgram.originalScript,
      oldProgram.altStack, oldProgram.flags,Some(lastCodeSeparator))
  }


  /** Updates the [[ScriptToken]]s in either the stack or script and the last [[org.bitcoins.core.script.crypto.OP_CODESEPARATOR]] index */
  def apply(oldProgram : ExecutionInProgressScriptProgram, tokens : Seq[ScriptToken], indicator: UpdateIndicator,
            lastCodeSeparator : Int) : ExecutionInProgressScriptProgram = {
    val updatedIndicator = ScriptProgram(oldProgram, tokens, indicator)
    updatedIndicator match {
      case e : ExecutionInProgressScriptProgram =>
        ScriptProgram(e,lastCodeSeparator)
      case _ : PreExecutionScriptProgram | _ : ExecutedScriptProgram =>
        throw new RuntimeException("We must have a ExecutionInProgressScriptProgram to update the last OP_CODESEPARATOR index")
    }
  }

  /** Updates the [[Stack]], [[Script]], [[AltStack]] of the given [[ScriptProgram]]. */
  def apply(oldProgram : ScriptProgram, stack : Seq[ScriptToken], script : Seq[ScriptToken], altStack : Seq[ScriptToken],
            updateIndicator: UpdateIndicator) : ScriptProgram = {
    val updatedProgramStack = ScriptProgram(oldProgram,stack, Stack)
    val updatedProgramScript = ScriptProgram(updatedProgramStack, script, Script)
    val updatedProgramAltStack = ScriptProgram(updatedProgramScript, altStack, AltStack)
    updatedProgramAltStack
  }

  /**
    * Creates a new [[ScriptProgram]] that can be used to verify if a [[Transaction]] at the given inputIndex
    * spends a given [[ScriptPubKey]] correctly. Assumes that the [[Script]] to be executed is the [[org.bitcoins.core.protocol.script.ScriptSignature]]
    * @param transaction the transaction that is being checked
    * @param scriptPubKey the scriptPubKey for which the input is spending
    * @param inputIndex the input's index inside of transaction which we are spending
    * @param flags the flags which we are enforcing inside of the script interpreter
    * @param amount the amount of [[CurrencyUnit]] we are spending in this input
    * @return the script program representing all of this information
    */
  def apply(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : UInt32,
            flags : Seq[ScriptFlag],
            amount: CurrencyUnit) : PreExecutionScriptProgram = {
    val script = transaction.inputs(inputIndex.toInt).scriptSignature.asm
    ScriptProgram(transaction,scriptPubKey,inputIndex,script.toList,flags, amount)
  }

  def apply(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : UInt32,
            stack : Seq[ScriptToken], script: Seq[ScriptToken], flags : Seq[ScriptFlag]): ScriptProgram = {
    val p = ScriptProgram(transaction,scriptPubKey,inputIndex,flags)
    ScriptProgram(p,stack,script)
  }

  def apply(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : UInt32, script : Seq[ScriptToken],
            flags : Seq[ScriptFlag], amount: CurrencyUnit) : PreExecutionScriptProgram = {
    val sigVersion = BitcoinScriptUtil.parseSigVersion(transaction,scriptPubKey,inputIndex)
    ScriptProgram(transaction,scriptPubKey,inputIndex,Nil,script,script,Nil,flags,sigVersion,amount)
  }

  def apply(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : UInt32, stack : Seq[ScriptToken],
            script : Seq[ScriptToken], flags : Seq[ScriptFlag], witness: ScriptWitness,
            amount: CurrencyUnit) : ScriptProgram = {
    val program = ScriptProgram(transaction,scriptPubKey,inputIndex,flags, amount)
    ScriptProgram(program,stack,script)
  }



  def apply(txSignatureComponent : TransactionSignatureComponent, stack : Seq[ScriptToken],
            script : Seq[ScriptToken]) : ScriptProgram = txSignatureComponent match {
    case b: BaseTransactionSignatureComponent =>
      ScriptProgram(b.transaction, b.scriptPubKey, b.inputIndex, stack, script, b.flags)
    case w: WitnessV0TransactionSignatureComponent =>
      ScriptProgram(w.transaction, w.scriptPubKey, w.inputIndex,
        stack, script, w.flags, w.witness, w.amount)
  }

  def apply(txSignatureComponent: TransactionSignatureComponent, stack: Seq[ScriptToken], script: Seq[ScriptToken],
            originalScript: Seq[ScriptToken]): ScriptProgram = txSignatureComponent match {
    case b : BaseTransactionSignatureComponent =>
      ExecutionInProgressScriptProgramImpl(b,stack.toList,
        script.toList,originalScript.toList,Nil,b.flags,None)
    case w: WitnessV0TransactionSignatureComponent =>
      ScriptProgram(w,stack,script,originalScript,Nil,w.flags,w.amount)
  }


  def apply(txSignatureComponent: TransactionSignatureComponent): PreExecutionScriptProgram = txSignatureComponent match {
    case b : BaseTransactionSignatureComponent =>
      ScriptProgram(b.transaction, b.scriptPubKey,
        b.inputIndex, b.flags)
    case w : WitnessV0TransactionSignatureComponent =>
      ScriptProgram(w.transaction, w.scriptPubKey,
        w.inputIndex, w.flags, w.amount)
  }

  /** Creates a fresh [[PreExecutionScriptProgram]] */
  def apply(transaction: Transaction, scriptPubKey: ScriptPubKey, inputIndex: UInt32, stack: Seq[ScriptToken],
            script: Seq[ScriptToken], originalScript: Seq[ScriptToken], altStack: Seq[ScriptToken],
            flags: Seq[ScriptFlag], sigVersion: SignatureVersion,
            amount: CurrencyUnit): PreExecutionScriptProgram = {
    val t = TransactionSignatureComponent(transaction,inputIndex,
      scriptPubKey,flags, amount,sigVersion)
    PreExecutionScriptProgramImpl(t,stack.toList,script.toList,originalScript.toList,altStack.toList,flags)
  }

  /** Creates a fresh instance of [[PreExecutionScriptProgram]] */
  def apply(txSigComponent: WitnessV0TransactionSignatureComponent, stack: Seq[ScriptToken], script: Seq[ScriptToken],
            originalScript: Seq[ScriptToken], altStack: Seq[ScriptToken], flags: Seq[ScriptFlag], amount: CurrencyUnit): PreExecutionScriptProgram = {
      ScriptProgram(txSigComponent.transaction, txSigComponent.scriptPubKey, txSigComponent.inputIndex, stack,
        script,originalScript, altStack,flags, txSigComponent.sigVersion, amount)
  }


  def apply(transaction: BaseTransaction, scriptPubKey: ScriptPubKey, inputIndex: UInt32, stack: Seq[ScriptToken],
            script: Seq[ScriptToken], originalScript: Seq[ScriptToken], altStack: Seq[ScriptToken],
            flags: Seq[ScriptFlag]): PreExecutionScriptProgram = {
    val t = TransactionSignatureComponent(transaction,inputIndex,scriptPubKey,flags)
    PreExecutionScriptProgramImpl(t,stack.toList,script.toList,originalScript.toList,altStack.toList,flags)
  }

  /** Creates a fresh instance of [[org.bitcoins.core.script.PreExecutionScriptProgram]] */
  def apply(transaction: Transaction, scriptPubKey: ScriptPubKey, inputIndex: UInt32,  flags: Seq[ScriptFlag]): PreExecutionScriptProgram = {
    val t = TransactionSignatureComponent(transaction,inputIndex,scriptPubKey,flags)
    val script = t.scriptSignature.asm
    PreExecutionScriptProgramImpl(t,Nil,script.toList,script.toList,Nil,t.flags)
  }

  /** Changes a [[ScriptProgram]] that is a [[ExecutionInProgressScriptProgram]] and changes it to an [[ExecutedScriptProgram]].*/
  def toExecutedProgram(executionInProgressScriptProgram: ExecutionInProgressScriptProgram) : ExecutedScriptProgram = {
    ExecutedScriptProgramImpl(executionInProgressScriptProgram.txSignatureComponent, executionInProgressScriptProgram.stack,
      executionInProgressScriptProgram.script,executionInProgressScriptProgram.originalScript,executionInProgressScriptProgram.altStack,
      executionInProgressScriptProgram.flags,None)
  }

  /** Changes a [[ScriptProgram]] that is a [[PreExecutionScriptProgram]] and changes it to an [[ExecutionInProgressScriptProgram]].*/
  def toExecutionInProgress(preExecutionScriptProgram: PreExecutionScriptProgram) : ExecutionInProgressScriptProgram = {
    toExecutionInProgress(preExecutionScriptProgram,None)
  }

  /** Changes a [[ScriptProgram]] that is a [[PreExecutionScriptProgram]] and changes it to an [[ExecutionInProgressScriptProgram]] given the stack state. */
  def toExecutionInProgress(preExecutionScriptProgram: PreExecutionScriptProgram, stack : Option[Seq[ScriptToken]]) : ExecutionInProgressScriptProgram = {
    stack match {
      case Some(stackTokens) => ExecutionInProgressScriptProgramImpl(preExecutionScriptProgram.txSignatureComponent,stackTokens.toList,preExecutionScriptProgram.script,
    preExecutionScriptProgram.originalScript,preExecutionScriptProgram.altStack,preExecutionScriptProgram.flags, None)
      case None =>
        ExecutionInProgressScriptProgramImpl(preExecutionScriptProgram.txSignatureComponent,preExecutionScriptProgram.stack,preExecutionScriptProgram.script,
          preExecutionScriptProgram.originalScript,preExecutionScriptProgram.altStack,preExecutionScriptProgram.flags, None)
    }
  }

}

