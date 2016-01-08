package org.scalacoin.script.locktime

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/8/16.
 */
trait LocktimeOperationFactory extends ScriptOperationFactory[LocktimeOperation] {

  override def operations = Seq(OP_CHECKLOCKTIMEVERIFY)

}

object LocktimeOperationFactory extends LocktimeOperationFactory
