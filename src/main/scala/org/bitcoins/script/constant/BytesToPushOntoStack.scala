package org.bitcoins.script.constant

import org.bitcoins.script.ScriptOperationFactory

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

object BytesToPushOntoStack extends ScriptOperationFactory[BytesToPushOntoStack] {
  /**
    * Represents that zero bytes need to be pushed onto the stack
    * this really means we need to push an empty byte vector on the stack
    */
  lazy val zero : BytesToPushOntoStack = apply(0).get

  override def operations : Seq[BytesToPushOntoStack] =
    (for { i <- 0 to 75 } yield BytesToPushOntoStackImpl(i)).toList

  def fromNumber(num : Int) : Option[BytesToPushOntoStack] = operations.find(_.opCode == num)

  def apply(num : Int) : Option[BytesToPushOntoStack] = fromNumber(num)
}
