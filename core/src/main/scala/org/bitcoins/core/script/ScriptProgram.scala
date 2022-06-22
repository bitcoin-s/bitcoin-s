package org.bitcoins.core.script

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{TaprootKeyPath, TaprootScriptPath}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control.{OP_ELSE, OP_ENDIF, OP_IF, OP_NOTIF}
import org.bitcoins.core.script.crypto.OP_CODESEPARATOR
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.script.result._
import org.bitcoins.core.util.BitcoinScriptUtil
import org.bitcoins.crypto.Sha256Digest

/** Created by chris on 2/3/16.
  */
sealed trait ScriptProgram {

  /** This contains all relevant information for hashing and checking a
    * [[org.bitcoins.core.protocol.script.ScriptSignature ScriptSignature]] for
    * a [[org.bitcoins.core.protocol.transaction.Transaction Transaction]].
    */
  def txSignatureComponent: TxSigComponent

  /** The current state of the stack for execution of the
    * [[org.bitcoins.core.script.ScriptProgram ScriptProgram]].
    */
  def stack: List[ScriptToken]

  /** The script operations that need to still be executed. */
  def script: List[ScriptToken]

  /** The original script that was given. */
  def originalScript: List[ScriptToken]

  /** The alternative stack is used in some Script op codes. */
  def altStack: List[ScriptToken]

  /** [[org.bitcoins.core.script.flag.ScriptFlag ScriptFlag]] that are run with the script.
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

  /** Sets a [[org.bitcoins.core.script.result.ScriptError ScriptError]] on a given
    * [[org.bitcoins.core.script.ScriptProgram ScriptProgram]].
    * @param error the error that the program hit while being executed in the script interpreter
    * @return the ExecutedScriptProgram with the given error set inside of the trait
    */
  def failExecution(error: ScriptError): ExecutedScriptProgram

  private def getTapscriptOpt: Option[TaprootScriptPath] = {
    txSignatureComponent match {
      case taprootTxSigComponent: TaprootTxSigComponent =>
        taprootTxSigComponent.witness match {
          case sp: TaprootScriptPath =>
            Some(sp)
          case _: TaprootKeyPath => None
        }
      case _: BaseTxSigComponent | _: WitnessTxSigComponentRebuilt |
          _: WitnessTxSigComponentP2SH | _: WitnessTxSigComponentRaw =>
        None
    }
  }

  /** Calculates the leaf hash if we have a tapscript, else returns None if we don't have a tapscript */
  def tapLeafHashOpt: Option[Sha256Digest] = {
    getTapscriptOpt.map { sp =>
      val hash = TaprootScriptPath.computeTapleafHash(
        TaprootScriptPath.TAPROOT_LEAF_TAPSCRIPT,
        sp.script)
      hash
    }
  }

  def getAnnexHashOpt: Option[Sha256Digest] = {
    getTapscriptOpt.flatMap(_.annexHashOpt)
  }

}

/** This represents a [[org.bitcoins.core.script.ScriptProgram ScriptProgram]]
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
      codeSeparatorTapscriptIdx = None,
      conditionalCounter = ConditionalCounter.empty
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
sealed trait StartedScriptProgram extends ScriptProgram {

  /** The index of the last code separator WITH push operations in the original script */
  def lastCodeSeparator: Option[Int]

  def taprootSerializationOptions: TaprootSerializationOptions = {
    val empty = TaprootSerializationOptions.empty
    val annex = empty.copy(annexHashOpt = getAnnexHashOpt)

    val lastCodeSeparatorU32 = calculateRealCodeSepIdx.map(UInt32(_))
    annex.copy(tapLeafHashOpt = tapLeafHashOpt,
               codeSeparatorPosOpt = lastCodeSeparatorU32)
  }

  /** Needs to translate [[OP_CODESEPARATOR]] idx WITH push ops
    * to [[OP_CODESEPARATOR]] index without push ops
    */
  private def calculateRealCodeSepIdx: Option[Int] = {

    lastCodeSeparator match {
      case Some(lastCodeSeparatorIdx) =>
        //map original indices to new indices
        val originalWithIndices = originalScript.zipWithIndex
        val scriptOpsWithIndices =
          originalWithIndices.filter(_._1.isInstanceOf[ScriptOperation])
        val opCodeIdxs =
          scriptOpsWithIndices.filter(_._2 == lastCodeSeparatorIdx).map(_._2)
        require(opCodeIdxs.length == 1, s"Should be exactly 1 OP_CODESEPARATOR")
        //calculate the offset without push operations
        val offset =
          opCodeIdxs.head - (originalScript.size - scriptOpsWithIndices.size) + 1
        println(s"offset=$offset")
        Some(offset)
      case None =>
        None
    }
  }
}

/** Implements the counting required for O(1) handling of conditionals in Bitcoin Script.
  * @see [[https://github.com/bitcoin/bitcoin/pull/16902]]
  *
  * @param trueCount The depth of OP_IFs/OP_NOTIFs we've entered on the true condition before the first false.
  * @param falseAndIgnoreCount The depth of OP_IFs/OP_NOTIFs we've entered after and including the first false condition.
  *                            Every OP_IF/OP_NOTIF adds to trueCount or falseAndIgnoreCount.
  *                           OP_ELSE has an effect only when falseAndIgnoreCount == 0 or 1, in which case it moves
  *                           1 from trueCount to falseAndIgnoreCount or vice versa.
  *                           OP_ENDIF subtracts one from either falseAndIgnoreCount or trueCount if falseAndIgnoreCount == 0.
  *                           trueCount + falseAndIgnoreCount represents the current depth in the conditional tree.
  *                           falseAndIgnoreCount == 0 represents whether operations should be executed.
  */
case class ConditionalCounter(trueCount: Int, falseAndIgnoreCount: Int) {
  require(trueCount >= 0, "Should have failed as unbalanced")
  require(falseAndIgnoreCount >= 0, "Should have failed as unbalanced")

  def noFalseEncountered: Boolean = {
    falseAndIgnoreCount == 0
  }

  def noTrueEncountered: Boolean = {
    trueCount == 0
  }

  def noConditionEncountered: Boolean = {
    noTrueEncountered && noFalseEncountered
  }

  def totalDepth: Int = {
    trueCount + falseAndIgnoreCount
  }

  /** Should be called for every OP_IF and OP_NOTIF with whether the first (true)
    * or second (false) branch should be taken.
    */
  def addCondition(condition: Boolean): ConditionalCounter = {
    if (!noFalseEncountered || !condition) {
      this.copy(falseAndIgnoreCount = falseAndIgnoreCount + 1)
    } else {
      this.copy(trueCount = trueCount + 1)
    }
  }

  /** Should be called on for every OP_ELSE.
    *
    * It is assumed that !noConditionEncountered
    */
  def invertCondition(): ConditionalCounter = {
    if (falseAndIgnoreCount > 1) {
      // Do nothing, we aren't in an execution now branch anyway
      this
    } else if (falseAndIgnoreCount == 1) {
      this.copy(trueCount = trueCount + 1, falseAndIgnoreCount = 0)
    } else { // Case falseAndIgnoreCount = 0, trueCount > 0
      this.copy(trueCount = trueCount - 1, falseAndIgnoreCount = 1)
    }
  }

  /** Should be called on for every OP_ENDIF.
    *
    * It is assumed that !noConditionEncountered
    */
  def removeCondition(): ConditionalCounter = {
    if (falseAndIgnoreCount > 0) {
      this.copy(falseAndIgnoreCount = falseAndIgnoreCount - 1)
    } else {
      this.copy(trueCount = trueCount - 1)
    }
  }
}

object ConditionalCounter {

  val empty: ConditionalCounter =
    ConditionalCounter(trueCount = 0, falseAndIgnoreCount = 0)
}

/** Type for a [[org.bitcoins.core.script.ScriptProgram ScriptProgram]] that is currently being
  * evaluated by the [[org.bitcoins.core.script.interpreter.ScriptInterpreter ScriptInterpreter]].
  *
  * @param lastCodeSeparator The index of the last [[org.bitcoins.core.script.crypto.OP_CODESEPARATOR OP_CODESEPARATOR]]
  * @param conditionalCounter Keeps track of where we are within a conditional tree.
  */
case class ExecutionInProgressScriptProgram(
    txSignatureComponent: TxSigComponent,
    stack: List[ScriptToken],
    script: List[ScriptToken],
    originalScript: List[ScriptToken],
    altStack: List[ScriptToken],
    flags: Seq[ScriptFlag],
    lastCodeSeparator: Option[Int],
    codeSeparatorTapscriptIdx: Option[Int],
    conditionalCounter: ConditionalCounter)
    extends StartedScriptProgram {

  def toExecutedProgram: ExecutedScriptProgram = {
    val errorOpt = if (conditionalCounter.totalDepth > 0) {
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
      lastCodeSeparator,
      codeSeparatorTapscriptIdx,
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
    conditionalCounter.noFalseEncountered
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
    this.copy(conditionalCounter = conditionalCounter.addCondition(condition))
  }

  /** Should be called on for every OP_ELSE */
  def invertCondition(): StartedScriptProgram = {
    if (conditionalCounter.noConditionEncountered) {
      this.failExecution(ScriptErrorUnbalancedConditional)
    } else {
      this.copy(conditionalCounter = conditionalCounter.invertCondition())
    }
  }

  /** Should be called on for every OP_ENDIF */
  def removeCondition(): StartedScriptProgram = {
    if (conditionalCounter.noConditionEncountered) {
      this.failExecution(ScriptErrorUnbalancedConditional)
    } else {
      this.copy(conditionalCounter = conditionalCounter.removeCondition())
    }
  }

  /** Removes the flags on the given [[org.bitcoins.core.script.ScriptProgram ScriptProgram]]
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

  def updateTapscriptCodeSeparatorIdx(
      newIdx: Int): ExecutionInProgressScriptProgram = {
    this.copy(codeSeparatorTapscriptIdx = Some(newIdx))
  }
}

/** Type for a [[org.bitcoins.core.script.ScriptProgram ScriptProgram]] that has been
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
    lastCodeSeparator: Option[Int],
    codeSeparatorTapscriptIdx: Option[Int],
    error: Option[ScriptError])
    extends StartedScriptProgram {

  override def failExecution(error: ScriptError): ExecutedScriptProgram = {
    this.copy(error = Some(error))
  }
}
