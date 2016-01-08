package org.scalacoin.script.control

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/8/16.
 */
trait ControlOperationsFactory extends ScriptOperationFactory[ControlOperations] {
  override def operations = Seq(OP_ELSE, OP_ENDIF, OP_IF, OP_NOP, OP_NOTIF, OP_RETURN, OP_VERIFY)
}

object ControlOperationsFactory extends ControlOperationsFactory
