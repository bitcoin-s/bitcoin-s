package org.bitcoins.script.stack

import org.bitcoins.script.ScriptOperationFactory
import org.bitcoins.script.constant.ScriptOperation

/**
 * Created by chris on 1/6/16.
 */
sealed trait StackOperation extends ScriptOperation



/**
 * Puts the input onto the top of the alt stack. Removes it from the main stack.
 */
case object OP_TOALTSTACK extends StackOperation {
  override def opCode = 107
}

/**
 * Puts the input onto the top of the main stack. Removes it from the alt stack.
 */
case object OP_FROMALTSTACK extends StackOperation {
  override def opCode = 108
}

/**
 * If the top stack value is not 0, duplicate it.
 */
case object OP_IFDUP extends StackOperation {
  override def opCode = 115
}

/**
 * Puts the number of stack items onto the stack.
 */
case object OP_DEPTH extends StackOperation {
  override def opCode = 116
}

/**
 * Removes the top stack item
 */
case object OP_DROP extends StackOperation {
  override def opCode = 	117
}

/**
 * 	Duplicates the top stack item.
 */
case object OP_DUP extends StackOperation {
  override def opCode = 118
}

/**
 * Removes the second-to-top stack item.
 */
case object OP_NIP extends StackOperation {
  override def opCode = 119
}

/**
 * 	Copies the second-to-top stack item to the top.
 */
case object OP_OVER extends StackOperation {
  override def opCode = 120
}

/**
 * The item n back in the stack is copied to the top.
 */
case object OP_PICK extends StackOperation {
  override def opCode = 121
}

/**
 * The item n back in the stack is moved to the top.
 */
case object OP_ROLL extends StackOperation {
  override def opCode = 122
}

/**
 * The top three items on the stack are rotated to the left.
 */
case object OP_ROT extends StackOperation {
  override def opCode = 123
}

/**
 * 	The top two items on the stack are swapped.
 */
case object OP_SWAP extends StackOperation {
  override def opCode = 124
}

/**
 * The item at the top of the stack is copied and inserted before the second-to-top item.
 */
case object OP_TUCK extends StackOperation {
  override def opCode = 125
}

/**
 * 	Removes the top two stack items.
 */
case object OP_2DROP extends StackOperation {
  override def opCode = 109
}

/**
 * Duplicates the top two stack items
 */
case object OP_2DUP extends StackOperation {
  override def opCode = 110
}

/**
 * Duplicates the top 3 stack items
 */
case object OP_3DUP extends StackOperation {
  override def opCode = 111
}

/**
 * Copies the pair of items two spaces back in the stack to the front.
 */
case object OP_2OVER extends StackOperation {
  override def opCode = 112
}

/**
 * The fifth and sixth items back are moved to the top of the stack.
 */
case object OP_2ROT extends StackOperation {
  override def opCode = 113
}

/**
 * 	Swaps the top two pairs of items.
 */
case object OP_2SWAP extends StackOperation {
  override def opCode = 114
}

object StackOperation extends ScriptOperationFactory[StackOperation] {
  override def operations = Seq(OP_TOALTSTACK,OP_FROMALTSTACK,OP_IFDUP,OP_DEPTH,
    OP_DEPTH,OP_DROP,OP_DUP,OP_NIP,OP_OVER,OP_ROLL,OP_ROT,OP_SWAP,OP_TUCK,OP_2DROP,OP_2DUP,
    OP_3DUP,OP_2OVER,OP_2ROT,OP_2SWAP, OP_PICK)
}


