package org.scalacoin.script.constant

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/9/16.
 */
trait ScriptNumberFactory extends ScriptOperationFactory[ScriptNumber] {

  override def operations = for { i <- 1 to 75 } yield ScriptNumberImpl(i)

}

object ScriptNumberFactory extends ScriptNumberFactory
