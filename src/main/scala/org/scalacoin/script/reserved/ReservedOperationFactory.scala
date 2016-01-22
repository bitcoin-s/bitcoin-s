package org.scalacoin.script.reserved

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/22/16.
 */
trait ReservedOperationFactory extends ScriptOperationFactory[ReservedOperation] {

  def operations = Seq(OP_RESERVED,OP_VER,OP_VERIF,OP_VERNOTIF,OP_RESERVED, OP_RESERVED1, OP_RESERVED2,
    OP_NOP1,OP_NOP3,OP_NOP4,OP_NOP5,OP_NOP6,OP_NOP7,OP_NOP8, OP_NOP9, OP_NOP10)
}

object ReservedOperationFactory extends ReservedOperationFactory