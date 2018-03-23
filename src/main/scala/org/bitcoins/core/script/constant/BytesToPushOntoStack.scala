package org.bitcoins.core.script.constant

import org.bitcoins.core.script.ScriptOperationFactory

/**
 * Created by chris on 1/9/16.
 * Represents a the amount of bytes that need to be pushed onto the stack
 */
trait BytesToPushOntoStack extends ScriptOperation

object BytesToPushOntoStack extends ScriptOperationFactory[BytesToPushOntoStack] {
  /**
   * Represents that zero bytes need to be pushed onto the stack
   * this really means we need to push an empty byte vector on the stack
   */
  lazy val zero: BytesToPushOntoStack = apply(0)

  private case class BytesToPushOntoStackImpl(num: Int) extends BytesToPushOntoStack {
    /*  //see the 'Constants; section in https://en.bitcoin.it/wiki/Script
      require(num >= -1 && num <= 75, "A valid script number is between 1 and 75, the number passed in was: " + num)*/
    require(num >= 0, "BytesToPushOntoStackImpl cannot be negative")
    override def opCode = num
  }

  override def operations: Seq[BytesToPushOntoStack] =
    (for { i <- 0 to 75 } yield BytesToPushOntoStackImpl(i))

  def fromNumber(num: Int): BytesToPushOntoStack = {
    if (num > 75) throw new IllegalArgumentException("We cannot have a BytesToPushOntoStack for greater than 75 bytes")
    else {
      val bytesToPushOntoStackOpt = operations.find(_.opCode == num)
      bytesToPushOntoStackOpt match {
        case Some(bytesToPushOntoStack) => bytesToPushOntoStack
        case None                       => throw new IllegalArgumentException("We cannot have a BytesToPushOntoStack for greater than 75 bytes")
      }
    }
  }

  def apply(num: Int): BytesToPushOntoStack = fromNumber(num)
}
