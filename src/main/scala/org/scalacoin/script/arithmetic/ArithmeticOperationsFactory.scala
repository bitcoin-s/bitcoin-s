package org.scalacoin.script.arithmetic

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/8/16.
 */
trait ArithmeticOperationsFactory extends ScriptOperationFactory[ArithmeticOperation] {

  override def operations = Seq(OP_0NOTEQUAL, OP_1ADD, OP_1SUB, OP_ABS, OP_ADD, OP_BOOLAND, OP_BOOLOR,
  OP_GREATERTHAN, OP_GREATERTHANOREQUAL, OP_LESSTHAN, OP_LESSTHANOREQUAL, OP_MAX, OP_MIN, OP_NEGATE,
  OP_NEGATE, OP_NOT, OP_NUMEQUAL, OP_NUMEQUALVERIFY, OP_NUMNOTEQUAL, OP_SUB, OP_WITHIN)
}

object ArithmeticOperationsFactory extends ArithmeticOperationsFactory