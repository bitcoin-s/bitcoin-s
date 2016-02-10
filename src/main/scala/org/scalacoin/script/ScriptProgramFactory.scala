package org.scalacoin.script

import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.ScriptProgramFactory.UpdateIndicator
import org.scalacoin.script.constant.ScriptToken

/**
 * Created by chris on 2/10/16.
 */
trait ScriptProgramFactory {

  //indicates whether the script or the stack needs to be updated
  sealed trait UpdateIndicator
  case object Stack extends UpdateIndicator
  case object Script extends UpdateIndicator
  case object FullScript extends UpdateIndicator
  case object AltStack extends UpdateIndicator

  def factory(oldProgram : ScriptProgram, valid : Boolean) : ScriptProgram = {
    ScriptProgramImpl(oldProgram.stack,oldProgram.script,oldProgram.transaction,
      oldProgram.altStack, oldProgram.fullScript, valid, oldProgram.lastCodeSeparator)
  }

  def factory(oldProgram : ScriptProgram, tx : Transaction) : ScriptProgram = {
    ScriptProgramImpl(oldProgram.stack,oldProgram.script,tx,
      oldProgram.altStack, oldProgram.fullScript,oldProgram.valid, oldProgram.lastCodeSeparator)
  }

  def factory(oldProgram : ScriptProgram, tokens : Seq[ScriptToken], indicator : UpdateIndicator) : ScriptProgram = {
    indicator match {
      case Stack => ScriptProgramImpl(tokens.toList, oldProgram.script, oldProgram.transaction,
        oldProgram.altStack, oldProgram.fullScript, oldProgram.valid, oldProgram.lastCodeSeparator)
      case Script => ScriptProgramImpl(oldProgram.stack, tokens.toList, oldProgram.transaction,
        oldProgram.altStack, oldProgram.fullScript, oldProgram.valid, oldProgram.lastCodeSeparator)
      case AltStack => ScriptProgramImpl(oldProgram.stack, oldProgram.script, oldProgram.transaction,
        tokens.toList, oldProgram.fullScript, oldProgram.valid, oldProgram.lastCodeSeparator)
      case FullScript => ScriptProgramImpl(oldProgram.stack, oldProgram.script, oldProgram.transaction,
        oldProgram.altStack, tokens.toList, oldProgram.valid, oldProgram.lastCodeSeparator)
    }
  }

  def factory(oldProgram : ScriptProgram, stackTokens : Seq[ScriptToken], scriptTokens : Seq[ScriptToken]) = {
    ScriptProgramImpl(stackTokens.toList,scriptTokens.toList,oldProgram.transaction, oldProgram.altStack,
      oldProgram.fullScript,
      valid = oldProgram.valid, lastCodeSeparator = oldProgram.lastCodeSeparator)
  }

  def factory(oldProgram : ScriptProgram, lastCodeSeparator : Int) : ScriptProgram = {
    ScriptProgramImpl(oldProgram.stack, oldProgram.script, oldProgram.transaction,
      oldProgram.altStack, oldProgram.fullScript, valid = oldProgram.valid, lastCodeSeparator = lastCodeSeparator)
  }

  def factory(oldProgram : ScriptProgram, tokens : Seq[ScriptToken], indicator: UpdateIndicator,
              lastCodeSeparator : Int) : ScriptProgram = {
    indicator match {
      case Stack => ScriptProgramImpl(tokens.toList, oldProgram.script, oldProgram.transaction,
        oldProgram.altStack, oldProgram.fullScript, oldProgram.valid, lastCodeSeparator)
      case Script => ScriptProgramImpl(oldProgram.stack, tokens.toList, oldProgram.transaction,
        oldProgram.altStack, oldProgram.fullScript, oldProgram.valid, lastCodeSeparator)
      case AltStack => ScriptProgramImpl(oldProgram.stack, oldProgram.script, oldProgram.transaction,
        tokens.toList, oldProgram.fullScript, oldProgram.valid, lastCodeSeparator)
      case FullScript => ScriptProgramImpl(oldProgram.stack, oldProgram.script, oldProgram.transaction,
        oldProgram.altStack, tokens.toList, oldProgram.valid, lastCodeSeparator)
    }
  }


  def factory(oldProgram : ScriptProgram, stack : Seq[ScriptToken], script : Seq[ScriptToken], valid : Boolean) = {
    ScriptProgramImpl(stack.toList, script.toList, oldProgram.transaction,
      oldProgram.altStack,oldProgram.fullScript,valid, oldProgram.lastCodeSeparator)
  }


  def factory(oldProgram : ScriptProgram, stack : Seq[ScriptToken], script : Seq[ScriptToken], that : Seq[ScriptToken],
               updateIndicator: UpdateIndicator) = {
    updateIndicator match {
      case FullScript =>
        ScriptProgramImpl(stack.toList,script.toList,oldProgram.transaction,
          oldProgram.altStack,that.toList,oldProgram.valid,oldProgram.lastCodeSeparator)
      case AltStack =>
        ScriptProgramImpl(stack.toList,script.toList,oldProgram.transaction,
          that.toList,oldProgram.fullScript,oldProgram.valid,oldProgram.lastCodeSeparator)
      case _ => ScriptProgramImpl(stack.toList,script.toList,oldProgram.transaction,
        oldProgram.altStack,oldProgram.fullScript,oldProgram.valid,oldProgram.lastCodeSeparator)
    }

  }

}

object ScriptProgramFactory extends ScriptProgramFactory
