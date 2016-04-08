package org.scalacoin.script

import org.scalacoin.crypto.{TransactionSignatureComponentFactory, TransactionSignatureComponent}
import org.scalacoin.protocol.script.ScriptPubKey
import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.ScriptProgramFactory.UpdateIndicator
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.script.flag.ScriptFlag

/**
 * Created by chris on 2/10/16.
 */
trait ScriptProgramFactory {

  private sealed case class ScriptProgramImpl(txSignatureComponent : TransactionSignatureComponent,
    stack : List[ScriptToken],script : List[ScriptToken], originalScript : List[ScriptToken], altStack : List[ScriptToken],
    flags : Seq[ScriptFlag],  isValid : Boolean = true, lastCodeSeparator : Int = 0) extends ScriptProgram



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
    ScriptProgramImpl(oldProgram.txSignatureComponent,
      oldProgram.stack,oldProgram.script, oldProgram.originalScript,
      oldProgram.altStack, oldProgram.flags, valid, oldProgram.lastCodeSeparator)
  }


  /**
   * Updates the program script verify flags
   * @param oldProgram
   * @param flags
   * @return
   */
  def factory(oldProgram : ScriptProgram, flags : Seq[ScriptFlag]) : ScriptProgram = {
    ScriptProgramImpl(oldProgram.txSignatureComponent,
      oldProgram.stack,oldProgram.script, oldProgram.originalScript,
      oldProgram.altStack, flags, oldProgram.isValid, oldProgram.lastCodeSeparator)
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
      case Stack => ScriptProgramImpl(oldProgram.txSignatureComponent,tokens.toList, oldProgram.script,
        oldProgram.originalScript,oldProgram.altStack, oldProgram.flags,
        oldProgram.isValid, oldProgram.lastCodeSeparator)
      case Script => ScriptProgramImpl(oldProgram.txSignatureComponent,
        oldProgram.stack, tokens.toList, oldProgram.originalScript,oldProgram.altStack,  oldProgram.flags,
        oldProgram.isValid, oldProgram.lastCodeSeparator)
      case AltStack => ScriptProgramImpl(oldProgram.txSignatureComponent,
        oldProgram.stack, oldProgram.script, oldProgram.originalScript,tokens.toList,oldProgram.flags,
        oldProgram.isValid, oldProgram.lastCodeSeparator)
    }
  }

  /**
   * Changes the stack tokens and script tokens in a ScriptProgram
   * @param oldProgram
   * @param stackTokens
   * @param scriptTokens
   * @return
   */
  def factory(oldProgram : ScriptProgram, stackTokens : Seq[ScriptToken], scriptTokens : Seq[ScriptToken]) : ScriptProgram = {
    val updatedStack = factory(oldProgram,stackTokens,Stack)
    val updatedScript = factory(updatedStack,scriptTokens,Script)
    updatedScript
  }

  /**
   * Updates the programs stack tokens, script tokens & script verify flags
   * @param oldProgram
   * @param stackTokens
   * @param scriptTokens
   * @param flags
   * @return
   */
  def factory(oldProgram : ScriptProgram, stackTokens : Seq[ScriptToken], scriptTokens : Seq[ScriptToken], flags : Seq[ScriptFlag]) : ScriptProgram = {
    val updatedStackAndScript = factory(oldProgram,stackTokens,scriptTokens)
    factory(updatedStackAndScript,flags)
  }

  /**
   * Updates the last OP_CODESEPARATOR index
   * @param oldProgram
   * @param lastCodeSeparator
   * @return
   */
  def factory(oldProgram : ScriptProgram, lastCodeSeparator : Int) : ScriptProgram = {
    ScriptProgramImpl(oldProgram.txSignatureComponent,
      oldProgram.stack, oldProgram.script, oldProgram.originalScript,
      oldProgram.altStack, oldProgram.flags,  isValid = oldProgram.isValid, lastCodeSeparator = lastCodeSeparator)
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
      case Stack => ScriptProgramImpl(oldProgram.txSignatureComponent,
        tokens.toList, oldProgram.script, oldProgram.originalScript, oldProgram.altStack, oldProgram.flags,  oldProgram.isValid, lastCodeSeparator)
      case Script => ScriptProgramImpl(oldProgram.txSignatureComponent,
        oldProgram.stack, tokens.toList, oldProgram.originalScript,oldProgram.altStack, oldProgram.flags,  oldProgram.isValid, lastCodeSeparator)
      case AltStack => ScriptProgramImpl(oldProgram.txSignatureComponent,
        oldProgram.stack, oldProgram.script,oldProgram.originalScript,
        tokens.toList, oldProgram.flags,  oldProgram.isValid, lastCodeSeparator)
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
  def factory(oldProgram : ScriptProgram, stack : Seq[ScriptToken], script : Seq[ScriptToken], valid : Boolean) : ScriptProgram = {
    val stackAndScriptUpdate = factory(oldProgram, stack,script)
    factory(stackAndScriptUpdate, valid)
  }


  /**
   * Updates the stack, script
   * @param oldProgram
   * @param stack
   * @param script
   * @param altStack
   * @param updateIndicator
   * @return
   */
  def factory(oldProgram : ScriptProgram, stack : Seq[ScriptToken], script : Seq[ScriptToken], altStack : Seq[ScriptToken],
               updateIndicator: UpdateIndicator) : ScriptProgram = {
    ScriptProgramImpl(oldProgram.txSignatureComponent,
      stack.toList,script.toList,oldProgram.originalScript,altStack.toList,oldProgram.flags,  oldProgram.isValid,oldProgram.lastCodeSeparator)
  }



  /**
   * Creates a new script program that can be used to verify if a transaction at the given inputIndex
   * spends a given scriptPubKey correctly. Assumes that the script to be executed is the
   * scriptSignature at the given input index
   * @param transaction the transaction that is being checked
   * @param scriptPubKey the scriptPubKey for which the input is spending
   * @param inputIndex the input's index inside of transaction which we are spending
   * @param flags the flags which we are enforcing inside of the script interpreter
   * @return the script program representing all of this information
   */
  def factory(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : Int,
              flags : Seq[ScriptFlag]) : ScriptProgram = {
    val script = transaction.inputs(inputIndex).scriptSignature.asm
    factory(transaction,scriptPubKey,inputIndex,script.toList,flags)
  }

  /**
   * Creates a new script program that can be used to verify if a transaction at the given inputIndex
   * spends a given scriptPubKey correctly
   * @param transaction the transaction that is being checked
   * @param scriptPubKey the scriptPubKey for which the input is spending
   * @param inputIndex the input's index inside of transaction which we are spending
   * @param script the script that we are currently executing
   * @param flags the flags which we are enforcing inside of the script interpreter
   * @return the script program representing all of this information
   */
  def factory(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : Int, script : Seq[ScriptToken],
              flags : Seq[ScriptFlag]) : ScriptProgram = {
    val txSignatureComponent = TransactionSignatureComponentFactory.factory(transaction,inputIndex,scriptPubKey,flags)
    ScriptProgramImpl(txSignatureComponent,List(),script.toList,script.toList,List(),flags)
  }


  /**
   * The intention for this factory function is to allow us to create a program that already has a stack state. This
   * is useful for after execution of a scriptSig, copying the stack into this program with the scriptPubKey read to
   * run inside the script variable
   * @param transaction the transaction being checked
   * @param scriptPubKey the scriptPubKey which the input is spending
   * @param inputIndex the input's index inside of the transaction we are spending
   * @param stack the current stack state of the program
   * @param script the script that we need to execute
   * @param flags the flags which we are enforcing inside of the script interpeter
   */
  def factory(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : Int, stack : Seq[ScriptToken],
              script : Seq[ScriptToken], flags : Seq[ScriptFlag]) : ScriptProgram = {
    val program = factory(transaction,scriptPubKey,inputIndex,script,flags)
    factory(program,stack,Stack)
  }


  /**
   * The intention for this factory function is to allow us to create a program that already has a stack state. This
   * is useful for after execution of a scriptSig, copying the stack into this program with the scriptPubKey read to
   * run inside the script variable
   * @param txSignatureComponent the relevant transaction information for execution of a script program
   * @param stack the current stack state of the program
   * @param script the script that we need to execute
   * @return
   */
  def factory(txSignatureComponent : TransactionSignatureComponent, stack : Seq[ScriptToken], script : Seq[ScriptToken]) : ScriptProgram = {
    factory(txSignatureComponent.transaction,txSignatureComponent.scriptPubKey,txSignatureComponent.inputIndex,
      stack,script,txSignatureComponent.flags)
  }

}

object ScriptProgramFactory extends ScriptProgramFactory
