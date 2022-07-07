package org.bitcoins.core.script.control

import org.bitcoins.core.protocol.script.{SigVersionWitnessV0, SignatureVersion}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.flag.ScriptFlagUtil
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.{
  ExecutionInProgressScriptProgram,
  StartedScriptProgram
}
import org.bitcoins.core.util._

/** Created by chris on 1/6/16.
  */
sealed abstract class ControlOperationsInterpreter {

  /** Factors out the similarities between OP_IF and OP_NOTIF */
  private def opConditional(conditional: ConditionalOperation)(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(conditional),
            s"Script top was not $conditional")

    if (program.isInExecutionBranch) {
      val sigVersion = program.txSignatureComponent.sigVersion
      val flags = program.flags
      val minimalIfEnabled = ScriptFlagUtil.minimalIfEnabled(flags)
      val stackTopOpt = program.stack.headOption

      stackTopOpt match {
        case None =>
          program.failExecution(ScriptErrorUnbalancedConditional)
        case Some(stackTop) =>
          if (isNotMinimalStackTop(stackTop, sigVersion, minimalIfEnabled)) {
            program.failExecution(ScriptErrorMinimalIf)
          } else {
            val stackTopTrue: Boolean = program.stackTopIsTrue

            val conditionToAdd = conditional match {
              case OP_IF    => stackTopTrue
              case OP_NOTIF => !stackTopTrue
            }

            program
              .updateStackAndScript(program.stack.tail, program.script.tail)
              .addCondition(conditionToAdd)
          }
      }
    } else {
      // Doesn't matter which condition we use here,
      // just that one gets added to keep track of depth
      program.updateScript(program.script.tail).addCondition(condition = true)
    }
  }

  /** Checks if the stack top is NOT minimially encoded */
  private def isNotMinimalStackTop(
      stackTop: ScriptToken,
      sigVersion: SignatureVersion,
      minimalIfEnabled: Boolean): Boolean = {
    //see: https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L447-L452
    //https://lists.linuxfoundation.org/pipermail/bitcoin-dev/2016-August/013014.html
    (sigVersion == SigVersionWitnessV0 && minimalIfEnabled
    && !BitcoinScriptUtil.isMinimalToken(stackTop))
  }

  /** If the top stack value is not 0, the statements are executed. The top stack value is removed. */
  def opIf(program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    opConditional(OP_IF)(program)
  }

  /** If the top stack value is 0, the statements are executed. The top stack value is removed. */
  def opNotIf(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    opConditional(OP_NOTIF)(program)
  }

  /** Evaluates the [[org.bitcoins.core.script.control.OP_ELSE OP_ELSE]] operator. */
  def opElse(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_ELSE),
            "First script opt must be OP_ELSE")
    program.updateScript(program.script.tail).invertCondition()
  }

  /** Evaluates an [[org.bitcoins.core.script.control.OP_ENDIF OP_ENDIF]] operator. */
  def opEndIf(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_ENDIF),
            "Script top must be OP_ENDIF")

    program.updateScript(program.script.tail).removeCondition()
  }

  /** Marks transaction as invalid. A standard way of attaching extra data to transactions is to add a zero-value output
    * with a [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]] consisting of
    * [[org.bitcoins.core.script.control.OP_RETURN OP_RETURN]] followed by exactly one pushdata op.
    * Such outputs are provably unspendable,
    * reducing their cost to the network. Currently it is usually considered non-standard (though valid) for
    * a transaction to
    * have more than one [[org.bitcoins.core.script.control.OP_RETURN OP_RETURN]] output or an
    * [[org.bitcoins.core.script.control.OP_RETURN OP_RETURN]] output with more than one pushdata op.
    */
  def opReturn(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_RETURN))
    program.failExecution(ScriptErrorOpReturn)
  }

  /** Marks [[org.bitcoins.core.protocol.transaction.Transaction Transaction]] as invalid if top stack value is not true. */
  def opVerify(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_VERIFY),
            "Script top must be OP_VERIFY")
    program.stack.nonEmpty match {
      case true =>
        if (program.stackTopIsFalse) program.failExecution(ScriptErrorVerify)
        else
          program.updateStackAndScript(program.stack.tail, program.script.tail)
      case false =>
        program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }
}

object ControlOperationsInterpreter extends ControlOperationsInterpreter
