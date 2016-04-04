package org.scalacoin.script.bitwise

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/8/16.
 */
trait BitwiseOperationsFactory extends ScriptOperationFactory[BitwiseOperation] {
  override def operations = Seq(OP_EQUAL, OP_EQUALVERIFY, OP_INVERT, OP_AND, OP_OR, OP_XOR)
}

object BitwiseOperationsFactory extends BitwiseOperationsFactory
