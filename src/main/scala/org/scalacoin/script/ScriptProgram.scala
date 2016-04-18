package org.scalacoin.script

import org.scalacoin.crypto.{TransactionSignatureComponentFactory, TransactionSignatureComponent}
import org.scalacoin.protocol.script.{ScriptSignature, ScriptPubKey}
import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.constant._
import org.scalacoin.script.error.ScriptError
import org.scalacoin.script.flag.ScriptFlag
import org.scalacoin.util.Factory

/**
  * Created by chris on 2/3/16.
 */
trait ScriptProgram {


  /**
   * This contains all relevant information for hashing and checking a signature for a bitcoin transaction
    *
    * @return
   */
  def txSignatureComponent : TransactionSignatureComponent

  /**
   * The current state of the stack for execution of the program
    *
    * @return
   */
  def stack : List[ScriptToken]

  /**
   * The script operations that need to still be executed
    *
    * @return
   */
  def script : List[ScriptToken]


  /**
   * The original script that was given t
    *
    * @return
   */
  def originalScript : List[ScriptToken]

  /**
   * The alternative stack is used in some Script op codes
    *
    * @return
   */
  def altStack : List[ScriptToken]

  /**
   * Flags that are run with the script
   * these flags indicate special conditions that a script needs to be run with
   * see: https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.h#L31
    *
    * @return
   */
  def flags : Seq[ScriptFlag]

  /**
   * A function to determine if the transaction is valid or not
    *
    * @return
   */
  def isValid : Boolean

  /**
   * The index of the last OP_CODESEPARATOR
    *
    * @return
   */
  def lastCodeSeparator : Int

  /**
   * Returns if the stack top is true
    *
    * @return
   */
  def stackTopIsTrue = !stackTopIsFalse

  /**
   * Returns if the stack top is false
    *
    * @return
   */
  def stackTopIsFalse : Boolean = {
    if (stack.headOption.isDefined &&
      (stack.head.hex == OP_FALSE.hex || stack.head.hex == ScriptNumberFactory.negativeZero.hex ||
        stack.head.hex == ScriptNumberFactory.zero.hex)) true
    else if (!stack.headOption.isDefined) true
    else false
  }

  /**
   * Indicates if the program has encountered a ScriptError in its execution
   * @return
   */
  def error : Option[ScriptError] = None
}

object ScriptProgram {

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
    *
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
    *
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
    *
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
    *
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
    *
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
    *
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
    *
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
    *
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
    *
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
    *
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
    *
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
    *
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
    *
    * @param txSignatureComponent the relevant transaction information for execution of a script program
    * @param stack the current stack state of the program
    * @param script the script that we need to execute
    * @return
    */
  def factory(txSignatureComponent : TransactionSignatureComponent, stack : Seq[ScriptToken], script : Seq[ScriptToken]) : ScriptProgram = {
    factory(txSignatureComponent.transaction,txSignatureComponent.scriptPubKey,txSignatureComponent.inputIndex,
      stack,script,txSignatureComponent.flags)
  }

  def apply(oldProgram : ScriptProgram, valid : Boolean) : ScriptProgram = factory(oldProgram, valid)
  def apply(oldProgram : ScriptProgram, flags : Seq[ScriptFlag]) : ScriptProgram = factory(oldProgram, flags)
  def apply(oldProgram : ScriptProgram, tokens : Seq[ScriptToken], indicator : UpdateIndicator) : ScriptProgram =
    factory(oldProgram, tokens, indicator)
  def apply(oldProgram : ScriptProgram, stackTokens : Seq[ScriptToken], scriptTokens : Seq[ScriptToken]) : ScriptProgram =
    factory(oldProgram, stackTokens, scriptTokens)
  def apply(oldProgram : ScriptProgram, stackTokens : Seq[ScriptToken], scriptTokens : Seq[ScriptToken], flags : Seq[ScriptFlag]) : ScriptProgram =
    factory(oldProgram, stackTokens, scriptTokens, flags)
  def apply(oldProgram : ScriptProgram, lastCodeSeparator : Int) : ScriptProgram = factory(oldProgram, lastCodeSeparator)
  def apply(oldProgram : ScriptProgram, tokens : Seq[ScriptToken], indicator: UpdateIndicator, lastCodeSeparator : Int) : ScriptProgram =
    factory(oldProgram, tokens, indicator, lastCodeSeparator)
  def apply(oldProgram : ScriptProgram, stack : Seq[ScriptToken], script : Seq[ScriptToken], valid : Boolean) : ScriptProgram =
    factory(oldProgram, stack, script, valid)
  def apply(oldProgram : ScriptProgram, stack : Seq[ScriptToken], script : Seq[ScriptToken], altStack : Seq[ScriptToken],
            updateIndicator: UpdateIndicator) : ScriptProgram = factory(oldProgram, stack, script, altStack, updateIndicator)
  def apply(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : Int,
            flags : Seq[ScriptFlag]) : ScriptProgram = factory(transaction, scriptPubKey, inputIndex, flags)
  def apply(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : Int, script : Seq[ScriptToken],
            flags : Seq[ScriptFlag]) : ScriptProgram = factory(transaction, scriptPubKey, inputIndex, script, flags)
  def apply(transaction: Transaction, scriptPubKey : ScriptPubKey, inputIndex : Int, stack : Seq[ScriptToken],
            script : Seq[ScriptToken], flags : Seq[ScriptFlag]) : ScriptProgram = factory(transaction, scriptPubKey, inputIndex, stack, script, flags)
  def apply(txSignatureComponent : TransactionSignatureComponent, stack : Seq[ScriptToken], script : Seq[ScriptToken]) : ScriptProgram =
    factory(txSignatureComponent, stack, script)
}

