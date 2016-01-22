package org.scalacoin.script.control

import org.scalacoin.script.constant._
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/6/16.
 */
class ControlOperationsInterpreterTest extends FlatSpec with MustMatchers with ControlOperationsInterpreter {

  "ControlOperationsInterpreter" must "have OP_VERIFY evaluate to true with '1' on the stack" in {
    val stack = List(ScriptTrue)
    val script = List(OP_VERIFY)
    val result = verify(stack,script)
    result._3 must be (true)
  }

  it must "have OP_VERIFY evaluate to false with '0' on the stack" in {
    val stack = List(ScriptFalse)
    val script = List(OP_VERIFY)
    val result = verify(stack,script)
    result._3 must be (false)
  }

  it must "fail for OP_VERIFY when there is nothing on the stack" in {
    intercept[IllegalArgumentException]  {
      val stack = List()
      val script = List(OP_VERIFY)
      val result = verify(stack,script)
    }
  }

  it must "fail for verify when there is nothing on the script stack" in {
    intercept[IllegalArgumentException]  {
      val stack = List(ScriptConstantImpl("1"))
      val script = List()
      val result = verify(stack,script)
    }
  }

  it must "find the indexes of our OP_ENDIF in a list of script tokens" in {
    val l = List(OP_ENDIF)
    findOP_ENDIF(l) must be (Some(0))
    findOP_ENDIF(List(OP_IF,OP_ELSE,OP_ENDIF,OP_ENDIF)) must be (Some(2))
    findOP_ENDIF(List(OP_0,OP_1,OP_2)) must be (None)

  }

  it must "find the indexes of OP_ELSE in a list of script tokens" in {
    findOP_ELSE(List(OP_ELSE)) must be (Some(0))
    findOP_ELSE(List(OP_IF,OP_ELSE,OP_ENDIF,OP_ELSE)) must be (Some(1))
    findOP_ELSE(List(OP_0,OP_1,OP_2)) must be (None)
  }

  it must "find the indexes of OP_ELSE and OP_ENDIF in a list of script tokens" in {
    findIndexesOP_ELSE_OP_ENDIF(List(OP_ELSE,OP_ENDIF)) must be (Some(0),Some(1))
    findIndexesOP_ELSE_OP_ENDIF(List(OP_IF, OP_ELSE,OP_ENDIF, OP_IF,OP_ELSE,OP_ENDIF)) must be (Some(1),Some(2))
    findIndexesOP_ELSE_OP_ENDIF(List(OP_IF,OP_IF)) must be (None,None)
  }
}
