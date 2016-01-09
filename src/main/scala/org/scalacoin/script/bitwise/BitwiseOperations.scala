package org.scalacoin.script.bitwise

import org.scalacoin.script.constant.ScriptOperation

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
