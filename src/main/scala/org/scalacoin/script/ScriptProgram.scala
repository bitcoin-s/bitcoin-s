package org.scalacoin.script

import org.scalacoin.protocol.script.{ScriptSignature, ScriptPubKey}
import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.constant.{OP_0, ScriptNumberImpl, ScriptFalse, ScriptToken}
import org.scalacoin.script.flag.ScriptFlag

/**
 * Created by chris on 2/3/16.
 */
trait ScriptProgram {
  /**
   * The transaction that is being run through the script interpreter
   * @return
   */
  def transaction : Transaction

  /**
   * The crediting scriptPubKey that the coins are being spent from
   * @return
   */
  def scriptPubKey : ScriptPubKey

  /**
   * The scriptSignature that is providing cryptographic proof that it can spend the scriptPubKey
   * @return
   */
  def scriptSignature : ScriptSignature = transaction.inputs(inputIndex).scriptSignature

  /**
   * The index in the sequence of inputs that is spending the scriptPubKey
   * @return
   */
  def inputIndex : Int

  /**
   * The current state of the stack for execution of the program
   * @return
   */
  def stack : List[ScriptToken]

  /**
   * The script operations that need to still be executed
   * @return
   */
  def script : List[ScriptToken]

  /**
   *
   * @return
   */
  def fullScript : List[ScriptToken] = (scriptSignature.asm ++ scriptPubKey.asm).toList


  /**
   * The alternative stack is used in some Script op codes
   * @return
   */
  def altStack : List[ScriptToken]

  /**
   * Flags that are run with the script
   * these flags indicate special conditions that a script needs to be run with
   * see: https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.h#L31
   * @return
   */
  def flags : Seq[ScriptFlag]

  /**
   * A function to determine if the transaction is valid or not
   * @return
   */
  def isValid : Boolean

  /**
   * The index of the last OP_CODE_SEPA
   * @return
   */
  def lastCodeSeparator : Int

  /**
   * Returns if the stack top is true
   * @return
   */
  def stackTopIsTrue = !stackTopIsFalse

  /**
   * Returns if the stack top is false
   * @return
   */
  def stackTopIsFalse : Boolean = {
    if (stack.headOption.isDefined &&
      (stack.head == ScriptFalse || stack.head == ScriptNumberImpl(0) || stack.head == OP_0)) true
    else if (!stack.headOption.isDefined) true
    else false
  }
}

case class ScriptProgramImpl(transaction : Transaction, scriptPubKey : ScriptPubKey, inputIndex : Int,
  stack : List[ScriptToken],script : List[ScriptToken], altStack : List[ScriptToken],
  flags : Seq[ScriptFlag], isValid : Boolean = true, lastCodeSeparator : Int = 0) extends ScriptProgram


