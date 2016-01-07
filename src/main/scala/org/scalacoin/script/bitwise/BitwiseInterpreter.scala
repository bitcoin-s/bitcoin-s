package org.scalacoin.script.bitwise

import org.scalacoin.script.ScriptOperation

/**
 * Created by chris on 1/6/16.
 */
trait BitwiseInterpreter  {

  def equal(stack : List[String], script : List[ScriptOperation]) : (List[String], List[ScriptOperation]) = {
    require(stack.size > 1, "Stack size must be 2 or more to compare the top two values")
    require(script.headOption.isDefined && script.head == OP_EQUAL, "Script operation must be OP_EQUAL")
    val newStack = stack match {
      case h :: h1 :: t => if (h == h1) "1" :: t else "0" :: t
    }
    (newStack,script.tail)

  }

  def equalVerify(stack : Seq[String], script : List[ScriptOperation]) : (Seq[String], Seq[ScriptOperation]) = ???
}
