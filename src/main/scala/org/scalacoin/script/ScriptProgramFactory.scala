package org.scalacoin.script

import org.scalacoin.protocol.script.ScriptPubKey
import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.ScriptProgramFactory.UpdateIndicator
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.script.flag.ScriptFlag

/**
 * Created by chris on 2/10/16.
 */
trait ScriptProgramFactory {

  //indicates whether the script or the stack needs to be updated
  sealed trait UpdateIndicator
  case object Stack extends UpdateIndicator
  case object Script extends UpdateIndicator
  case object AltStack extends UpdateIndicator

  /**
   * Changes the validity of a script program
   * @param oldProgram
   * @param valid
   * @return
   */
  def factory(oldProgram : ScriptProgram, valid : Boolean) : ScriptProgram = {
    ScriptProgramImpl(oldProgram.transaction,oldProgram.scriptPubKey,oldProgram.inputIndex,
      oldProgram.stack,oldProgram.script, oldProgram.altStack, oldProgram.flags, valid, oldProgram.lastCodeSeparator)
  }


  /**
   * Changes the tokens in either the Stack or the Script dependong in the indicactor
   * @param oldProgram
   * @param tokens
   * @param indicator
   * @return
   */
  def factory(oldProgram : ScriptProgram, tokens : Seq[ScriptToken], indicator : UpdateIndicator) : ScriptProgram = {
    indicator match {
      case Stack => ScriptProgramImpl(oldProgram.transaction,oldProgram.scriptPubKey,
        oldProgram.inputIndex,tokens.toList, oldProgram.script,
        oldProgram.altStack, oldProgram.flags, oldProgram.isValid, oldProgram.lastCodeSeparator)
      case Script => ScriptProgramImpl(oldProgram.transaction, oldProgram.scriptPubKey, oldProgram.inputIndex,
        oldProgram.stack, tokens.toList, oldProgram.altStack, oldProgram.flags, oldProgram.isValid, oldProgram.lastCodeSeparator)
      case AltStack => ScriptProgramImpl(oldProgram.transaction,oldProgram.scriptPubKey, oldProgram.inputIndex,
        oldProgram.stack, oldProgram.script, tokens.toList, oldProgram.flags, oldProgram.isValid, oldProgram.lastCodeSeparator)
    }
  }

  /**
   * Changes the stack tokens and script tokens in a ScriptProgram
   * @param oldProgram
   * @param stackTokens
   * @param scriptTokens
   * @return
   */
  def factory(oldProgram : ScriptProgram, stackTokens : Seq[ScriptToken], scriptTokens : Seq[ScriptToken]) = {
    ScriptProgramImpl(oldProgram.transaction, oldProgram.scriptPubKey, oldProgram.inputIndex,
      stackTokens.toList,scriptTokens.toList, oldProgram.altStack, oldProgram.flags,
      oldProgram.isValid,oldProgram.lastCodeSeparator)
  }

  /**
   * Updates the programs stack tokens, script tokens & script verify flags
   * @param oldProgram
   * @param stackTokens
   * @param scriptTokens
   * @param flags
   * @return
   */
  def factory(oldProgram : ScriptProgram, stackTokens : Seq[ScriptToken], scriptTokens : Seq[ScriptToken], flags : Seq[ScriptFlag]) = {
    ScriptProgramImpl(oldProgram.transaction, oldProgram.scriptPubKey, oldProgram.inputIndex,
      stackTokens.toList,scriptTokens.toList, oldProgram.altStack, flags,
      oldProgram.isValid,oldProgram.lastCodeSeparator)
  }

  /**
   * Updates the last OP_CODESEPARATOR index
   * @param oldProgram
   * @param lastCodeSeparator
   * @return
   */
  def factory(oldProgram : ScriptProgram, lastCodeSeparator : Int) : ScriptProgram = {
    ScriptProgramImpl(oldProgram.transaction, oldProgram.scriptPubKey, oldProgram.inputIndex,
      oldProgram.stack, oldProgram.script,
      oldProgram.altStack, oldProgram.flags, isValid = oldProgram.isValid, lastCodeSeparator = lastCodeSeparator)
  }

  /**
   * Updates the tokens in either the stack or script and the last OP_CODESEPARATOR index
   * @param oldProgram
   * @param tokens
   * @param indicator
   * @param lastCodeSeparator
   * @return
   */
  def factory(oldProgram : ScriptProgram, tokens : Seq[ScriptToken], indicator: UpdateIndicator,
              lastCodeSeparator : Int) : ScriptProgram = {
    indicator match {
      case Stack => ScriptProgramImpl(oldProgram.transaction, oldProgram.scriptPubKey, oldProgram.inputIndex,
        tokens.toList, oldProgram.script, oldProgram.altStack, oldProgram.flags, oldProgram.isValid, lastCodeSeparator)
      case Script => ScriptProgramImpl(oldProgram.transaction, oldProgram.scriptPubKey, oldProgram.inputIndex,
        oldProgram.stack, tokens.toList, oldProgram.altStack, oldProgram.flags, oldProgram.isValid, lastCodeSeparator)
      case AltStack => ScriptProgramImpl(oldProgram.transaction, oldProgram.scriptPubKey,
        oldProgram.inputIndex,oldProgram.stack, oldProgram.script,
        tokens.toList, oldProgram.flags, oldProgram.isValid, lastCodeSeparator)
    }
  }


  /**
   * Updates the stack, script and validity of a script program
   * @param oldProgram
   * @param stack
   * @param script
   * @param valid
   * @return
   */
  def factory(oldProgram : ScriptProgram, stack : Seq[ScriptToken], script : Seq[ScriptToken], valid : Boolean) = {
    ScriptProgramImpl(oldProgram.transaction, oldProgram.scriptPubKey, oldProgram.inputIndex,
      stack.toList, script.toList, oldProgram.altStack,oldProgram.flags, valid, oldProgram.lastCodeSeparator)
  }


  /**
   * Updates the stack, script
   * @param oldProgram
   * @param stack
   * @param script
   * @param that
   * @param updateIndicator
   * @return
   */
  def factory(oldProgram : ScriptProgram, stack : Seq[ScriptToken], script : Seq[ScriptToken], that : Seq[ScriptToken],
               updateIndicator: UpdateIndicator) = {
    updateIndicator match {
      case AltStack =>
        ScriptProgramImpl(oldProgram.transaction, oldProgram.scriptPubKey, oldProgram.inputIndex,
          stack.toList,script.toList,that.toList, oldProgram.flags, oldProgram.isValid,oldProgram.lastCodeSeparator)
      case _ => ScriptProgramImpl(oldProgram.transaction, oldProgram.scriptPubKey, oldProgram.inputIndex,
        stack.toList,script.toList, oldProgram.altStack,oldProgram.flags,oldProgram.isValid,oldProgram.lastCodeSeparator)
    }

  }

  /**
   * Creates a new script program that can be used to verify if a transaction at the given inputIndex
   * spends a given scriptPubKey correctly
   * @param transaction
   * @param scriptPubKey
   * @param inputIndex
   * @return
   */
  def factory(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : Int,flags : Seq[ScriptFlag]) = {
    val script = transaction.inputs(inputIndex).scriptSignature.asm ++ scriptPubKey.asm
    ScriptProgramImpl(transaction,scriptPubKey,inputIndex,List(),script.toList,List(),flags)
  }

}

object ScriptProgramFactory extends ScriptProgramFactory
