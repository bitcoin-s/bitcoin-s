package org.scalacoin.script.constant

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/9/16.
 * Represents a the amount of bytes that need to be pushed onto the stack
 */
trait BytesToPushOntoStack extends ScriptOperation

case class BytesToPushOntoStackImpl(num : Int) extends BytesToPushOntoStack {
/*  //see the 'Constants; section in https://en.bitcoin.it/wiki/Script
  require(num >= -1 && num <= 75, "A valid script number is between 1 and 75, the number passed in was: " + num)*/
  require(num >= 0, "BytesToPushOntoStackImpl cannot be negative")
  override def opCode = num
}



