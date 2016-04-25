package org.scalacoin.script.constant

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/9/16.
 */
trait BytesToPushOntoStackFactory extends ScriptOperationFactory[BytesToPushOntoStack] {

  /**
   * Represents that zero bytes need to be pushed onto the stack
   * this really means we need to push an empty byte vector on the stack
   */
  lazy val zero : BytesToPushOntoStack = factory(0).get

  override def operations : Seq[BytesToPushOntoStack] =
      (for { i <- 0 to 75 } yield BytesToPushOntoStackImpl(i)).toList

  def factory(num : Int) : Option[BytesToPushOntoStack] = fromNumber(num)


  def fromNumber(num : Int) : Option[BytesToPushOntoStack] = operations.find(_.opCode == num)
}
@deprecated
object BytesToPushOntoStackFactory extends BytesToPushOntoStackFactory
