package org.scalacoin.script.bitwise

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/8/16.
 */
trait BitwiseOperationsFactory extends ScriptOperationFactory[BitwiseOperation] {
  override def operations = Seq(OP_EQUAL, OP_EQUALVERIFY)
}

object BitwiseOperationsFactory extends BitwiseOperationsFactory
