package org.scalacoin.script

/**
 * Created by chris on 1/6/16.
 */

trait ScriptOperation {
  def opCode : Int
  def hex : String = Integer.toHexString(opCode)
}

sealed trait Constant extends ScriptOperation


/**
 * Represent a pubkey or hash of a pub key on our stack
 *
 * @param str
 */
case class ConstantImpl(str : String) extends ScriptOperation  {
  //TODO: Get around this op code some how, constants don't have op codes
  override def opCode = -1

}
/**
 * An empty array of bytes is pushed onto the stack. (This is not a no-op: an item is added to the stack.)
 */
case object OP_0 extends Constant {
  override def opCode = 0
}
/**
 * An empty array of bytes is pushed onto the stack. (This is not a no-op: an item is added to the stack.)
 */
case object OP_FALSE extends Constant {
  override def opCode = OP_0.opCode
}

/**
 * 	The next byte contains the number of bytes to be pushed onto the stack.
 */
case object OP_PUSHDATA1 extends Constant {
  override def opCode = 76
}

/**
 * 	The next two bytes contain the number of bytes to be pushed onto the stack.
 */
case object OP_PUSHDATA2 extends Constant {
  override def opCode = 77
}

/**
 * The next four bytes contain the number of bytes to be pushed onto the stack.
 */
case object OP_PUSHDATA4 extends Constant {
  override def opCode = 78
}

/**
 * The number -1 is pushed onto the stack.
 */
case object OP_1NEGATE extends Constant {
  override def opCode = 79
}

/**
 * The number 1 is pushed onto the stack.
 */
case object OP_TRUE extends Constant {
  override def opCode = 81
}

/**
 * The number 1 is pushed onto the stack.
 */
case object OP_1 extends Constant {
  override def opCode = OP_TRUE.opCode
}

/**
 * The number 2 is pushed onto the stack.
 */
case object OP_2 extends Constant {
  override def opCode = 82
}

/**
 * The number 3 is pushed onto the stack.
 */
case object OP_3 extends Constant {
  override def opCode = 83
}

/**
 * The number 4 is pushed onto the stack.
 */
case object OP_4 extends Constant {
  override def opCode = 84
}

/**
 * The number 5 is pushed onto the stack.
 */
case object OP_5 extends Constant {
  override def opCode = 85
}

/**
 * The number 6 is pushed onto the stack.
 */
case object OP_6 extends Constant {
  override def opCode = 86
}

/**
 * The number 7 is pushed onto the stack.
 */
case object OP_7 extends Constant {
  override def opCode = 87
}

/**
 * The number 8 is pushed onto the stack.
 */
case object OP_8 extends Constant {
  override def opCode = 88
}

/**
 * The number 9 is pushed onto the stack.
 */
case object OP_9 extends Constant {
  override def opCode = 89
}

/**
 * The number 10 is pushed onto the stack.
 */
case object OP_10 extends Constant {
  override def opCode = 90
}

/**
 * The number 11 is pushed onto the stack.
 */
case object OP_11 extends Constant {
  override def opCode = 91
}

/**
 * The number 12 is pushed onto the stack.
 */
case object OP_12 extends Constant {
  override def opCode = 92
}

/**
 * The number 13 is pushed onto the stack.
 */
case object OP_13 extends Constant {
  override def opCode = 93
}

/**
 * The number 14 is pushed onto the stack.
 */
case object OP_14 extends Constant {
  override def opCode = 94
}

/**
 * The number 15 is pushed onto the stack.
 */
case object OP_15 extends Constant {
  override def opCode = 95
}

/**
 * The number 16 is pushed onto the stack.
 */
case object OP_16 extends Constant {
  override def opCode = 96
}




