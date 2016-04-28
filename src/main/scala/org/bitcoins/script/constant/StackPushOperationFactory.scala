package org.bitcoins.script.constant


/**
 * Created by chris on 3/28/16.
 */
trait StackPushOperationFactory {


  /**
   * Gives back all of the script operations that can push data onto the stack
   * The operations are determined according to BIP62
   * https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#push-operators
   * @return
   */
  def operations = Seq(OP_PUSHDATA1, OP_PUSHDATA2, OP_PUSHDATA4) ++ BytesToPushOntoStack.operations ++
    Seq(OP_0,OP_1,OP_1NEGATE, OP_2,OP_3,OP_4,OP_5,OP_6,OP_7,OP_8,
    OP_9,OP_10,OP_11,OP_12,OP_13,OP_14,OP_15,OP_16,OP_FALSE,OP_TRUE)

  /**
   * Determines if the given token is a stack push operation
   * @param token the token to be checked to see if it is a stack push operation
   * @return a boolean indiciating if the given token was a stack push operation
   */
  def isPushOperation(token : ScriptToken) : Boolean = operations.contains(token)

}

object StackPushOperationFactory extends StackPushOperationFactory
