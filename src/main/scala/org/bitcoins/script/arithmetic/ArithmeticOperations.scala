package org.bitcoins.script.arithmetic

import org.bitcoins.script.ScriptOperationFactory
import org.bitcoins.script.constant.ScriptOperation

/**
 * Created by chris on 1/6/16.
 */
sealed trait ArithmeticOperation extends ScriptOperation


/**
 * 1 is added to the input.
 */
case object OP_1ADD extends ArithmeticOperation {
  override def opCode = 139
}

/**
 * 1 is subtracted from the input.
 */
case object OP_1SUB extends ArithmeticOperation {
  override def opCode = 140
}

/**
 * The sign of the input is flipped.
 */
case object OP_NEGATE extends ArithmeticOperation {
  override def opCode = 143
}

/**
 * The input is made positive.
 */
case object OP_ABS extends ArithmeticOperation {
  override def opCode = 144
}

/**
 * If the input is 0 or 1, it is flipped. Otherwise the output will be 0.
 */

case object OP_NOT extends ArithmeticOperation {
  override def opCode = 145
}

/**
 * Returns 0 if the input is 0. 1 otherwise.
 */
case object OP_0NOTEQUAL extends ArithmeticOperation {
  override def opCode = 146
}

/**
 * 	a is added to b.
 */
case object OP_ADD extends ArithmeticOperation {
  override def opCode = 147
}

/**
 * b is subtracted from a.
 */
case object OP_SUB extends ArithmeticOperation {
  override def opCode = 148
}

/**
 * 	If both a and b are not 0, the output is 1. Otherwise 0.
 */
case object OP_BOOLAND extends ArithmeticOperation {
  override def opCode = 154
}

/**
 * If a or b is not 0, the output is 1. Otherwise 0.
 */
case object OP_BOOLOR extends ArithmeticOperation {
  override def opCode = 155
}

/**
 * 	Returns 1 if the numbers are equal, 0 otherwise.
 */
case object OP_NUMEQUAL extends ArithmeticOperation {
  override def opCode = 156
}

/**
 * Same as OP_NUMEQUAL, but runs OP_VERIFY afterward.
 */
case object OP_NUMEQUALVERIFY extends ArithmeticOperation {
  override def opCode= 157
}

/**
 * Returns 1 if the numbers are not equal, 0 otherwise.
 */
case object OP_NUMNOTEQUAL extends ArithmeticOperation {
  override def opCode = 158
}

/**
 * Returns 1 if a is less than b, 0 otherwise.
 */
case object OP_LESSTHAN extends ArithmeticOperation {
  override def opCode = 159
}

/**
 * Returns 1 if a is greater than b, 0 otherwise.
 */
case object OP_GREATERTHAN extends ArithmeticOperation {
  override def opCode = 160
}

/**
 * 	Returns 1 if a is less than or equal to b, 0 otherwise.
 */
case object OP_LESSTHANOREQUAL extends ArithmeticOperation {
  override def opCode = 161
}

/**
 * 		Returns 1 if a is greater than or equal to b, 0 otherwise.
 */
case object OP_GREATERTHANOREQUAL extends ArithmeticOperation {
  override def opCode = 162
}

/**
 * 	Returns the smaller of a and b.
 */
case object OP_MIN extends ArithmeticOperation {
  override def opCode = 163
}

/**
 * 	Returns the larger of a and b.
 */
case object OP_MAX extends ArithmeticOperation {
  override def opCode = 164
}

/**
 * Returns 1 if x is within the specified range (left-inclusive), 0 otherwise.
 */
case object OP_WITHIN  extends ArithmeticOperation {
  override def opCode = 165
}


//currently disabled operations

/**
 * The input is multiplied by 2. disabled.
 */
case object OP_2MUL extends ArithmeticOperation {
  override def opCode = 141
}

/**
 * The input is divided by 2. disabled.
 */
case object OP_2DIV extends ArithmeticOperation {
  override def opCode = 142
}

/**
 * a is multiplied by b. disabled.
 */
case object OP_MUL extends ArithmeticOperation {
  override def opCode = 149
}

/**
 * 	a is divided by b. disabled.
 */
case object OP_DIV extends ArithmeticOperation {
  override def opCode = 150
}

/**
 * Returns the remainder after dividing a by b. disabled.
 */
case object OP_MOD extends ArithmeticOperation {
  override def opCode = 151
}

/**
 * Shifts a left b bits, preserving sign. disabled.
 */
case object  OP_LSHIFT extends ArithmeticOperation {
  override def opCode = 152
}

/**
 * Shifts a right b bits, preserving sign. disabled.
 */
case object  OP_RSHIFT extends ArithmeticOperation {
  override def opCode = 153
}

object ArithmeticOperation extends ScriptOperationFactory[ArithmeticOperation] {
  override def operations = Seq(OP_0NOTEQUAL, OP_1ADD, OP_1SUB, OP_ABS, OP_ADD, OP_BOOLAND, OP_BOOLOR,
    OP_GREATERTHAN, OP_GREATERTHANOREQUAL, OP_LESSTHAN, OP_LESSTHANOREQUAL, OP_MAX, OP_MIN, OP_NEGATE,
    OP_NEGATE, OP_NOT, OP_NUMEQUAL, OP_NUMEQUALVERIFY, OP_NUMNOTEQUAL, OP_SUB, OP_WITHIN,
    OP_2MUL,OP_2DIV,OP_MUL,OP_DIV, OP_MOD, OP_LSHIFT, OP_RSHIFT)
}


