package org.bitcoins.script.bitwise

import org.bitcoins.script.ScriptOperationFactory
import org.bitcoins.script.constant.ScriptOperation

/**
 * Created by chris on 1/6/16.
 */
sealed trait BitwiseOperation extends ScriptOperation

/**
 * Returns 1 if the inputs are exactly equal, 0 otherwise.
 */
case object OP_EQUAL extends BitwiseOperation {
  override def opCode = 135
}

/**
 * Same as OP_EQUAL, but runs OP_VERIFY afterward.
 */
case object OP_EQUALVERIFY extends BitwiseOperation {
  override def opCode = 136
}

/**
 * Flips all of the bits in the input. disabled.
 */
case object OP_INVERT extends BitwiseOperation {
  override def opCode = 131
}

/**
 * Boolean and between each bit in the inputs. disabled.
 */
case object OP_AND extends BitwiseOperation {
  override def opCode = 132
}

/**
 * Boolean or between each bit in the inputs. disabled.
 */
case object OP_OR extends BitwiseOperation {
  override def opCode = 133
}

/**
 * Boolean exclusive or between each bit in the inputs. disabled.
 */
case object OP_XOR extends BitwiseOperation {
  override def opCode = 134
}

object BitwiseOperation extends ScriptOperationFactory[BitwiseOperation] {
  override def operations = Seq(OP_EQUAL, OP_EQUALVERIFY, OP_INVERT, OP_AND, OP_OR, OP_XOR)
}