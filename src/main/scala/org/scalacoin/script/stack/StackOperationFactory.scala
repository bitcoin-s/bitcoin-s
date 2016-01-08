package org.scalacoin.script.stack

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/8/16.
 */
trait StackOperationFactory extends ScriptOperationFactory[StackOperation]  {

  override def operations = Seq(OP_TOALTSTACK,OP_FROMALTSTACK,OP_IFDUP,OP_DEPTH,
    OP_DEPTH,OP_DROP,OP_DUP,OP_NIP,OP_OVER,OP_ROLL,OP_ROT,OP_SWAP,OP_TUCK,OP_2DROP,OP_2DUP,
    OP_3DUP,OP_2OVER,OP_2ROT,OP_2SWAP)

}

object StackOperationFactory extends StackOperationFactory
