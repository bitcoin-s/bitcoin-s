package org.scalacoin.script.constant

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/9/16.
 */
trait ScriptNumberFactory extends ScriptOperationFactory[ScriptNumber] {

  override def operations : Seq[ScriptNumber] =  OP_0 :: (for { i <- 1 to 75 } yield ScriptNumberImpl(i)).toList

  def factory(num : Int) : Option[ScriptNumber] = operations.find(_.opCode == num)

}

object ScriptNumberFactory extends ScriptNumberFactory
