package org.scalacoin.script

import org.scalacoin.crypto.{TransactionSignatureComponent}
import org.scalacoin.protocol.script.{ScriptSignature, ScriptPubKey}
import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.constant._
import org.scalacoin.script.flag.ScriptFlag

/**
 * Created by chris on 2/3/16.
 */
trait ScriptProgram {


  /**
   * This contains all relevant information for hashing and checking a signature for a bitcoin transaction
   * @return
   */
  def txSignatureComponent : TransactionSignatureComponent

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
   * The original script that was given t
   * @return
   */
  def originalScript : List[ScriptToken]

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
   * The index of the last OP_CODESEPARATOR
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
      (stack.head == ScriptFalse || stack.head == ScriptNumberFactory.zero || stack.head == ScriptNumberFactory.negativeZero
        || stack.head == OP_0 || stack.head == OP_FALSE)) true
    else if (!stack.headOption.isDefined) true
    else false
  }
}


