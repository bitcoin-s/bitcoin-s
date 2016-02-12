package org.scalacoin.script.constant

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/9/16.
 */
trait BytesToPushOntoStackFactory extends ScriptOperationFactory[BytesToPushOntoStack] {

  override def operations : Seq[BytesToPushOntoStack] =
      (for { i <- 0 to 75 } yield BytesToPushOntoStackImpl(i)).toList

  def factory(num : Int) : Option[BytesToPushOntoStack] = operations.find(_.opCode == num)

}

object BytesToPushOntoStackFactory extends BytesToPushOntoStackFactory
