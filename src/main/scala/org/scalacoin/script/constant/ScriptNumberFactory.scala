package org.scalacoin.script.constant

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/9/16.
 */
trait ScriptNumberFactory extends ScriptOperationFactory[ScriptNumber] {

  override def operations : Seq[ScriptNumber] =
    Seq(OP_0,OP_1,OP_2,OP_3,OP_4,OP_5,OP_6,OP_7,OP_8,OP_9,OP_10,OP_11,OP_12,OP_13,OP_14,OP_15,OP_16) ++
      (for { i <- -1 to 75 } yield ScriptNumberImpl(i)).toList

  def factory(num : Int) : Option[ScriptNumber] = operations.find(_.opCode == num)

}

object ScriptNumberFactory extends ScriptNumberFactory
