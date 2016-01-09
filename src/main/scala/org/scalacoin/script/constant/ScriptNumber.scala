package org.scalacoin.script.constant

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/9/16.
 */
trait ScriptNumber extends ScriptOperation

case class ScriptNumberImpl(num : Int) extends ScriptNumber {
  //see the 'Constants; section in https://en.bitcoin.it/wiki/Script
  require(num >= 1 && num <= 75, "A valid script number is between 1 and 75, the nubmer passed in was: " + num)

  override def opCode = num
}



