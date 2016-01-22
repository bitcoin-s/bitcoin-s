package org.scalacoin.script.splice

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/22/16.
 */
object SpliceOperationsFactory extends ScriptOperationFactory[SpliceOperation] {

  def operations = Seq(OP_CAT, OP_LEFT, OP_RIGHT, OP_SIZE, OP_SUBSTR)
}
