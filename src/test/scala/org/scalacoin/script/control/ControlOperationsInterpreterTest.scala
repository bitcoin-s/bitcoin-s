package org.scalacoin.script.control

import org.scalacoin.script.constant._
import org.scalacoin.script.reserved.{OP_VER, OP_RESERVED}
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
    findOpEndIf(l) must be (Some(0))
    findOpEndIf(List(OP_IF,OP_ELSE,OP_ENDIF,OP_ENDIF)) must be (Some(2))
    findOpEndIf(List(OP_0,OP_1,OP_2)) must be (None)
    findOpEndIf(List(OP_IF, OP_RESERVED, OP_ENDIF, OP_1)) must be (Some(2))

  }

  it must "find the indexes of OP_ELSE in a list of script tokens" in {
    findOpElse(List(OP_ELSE)) must be (Some(0))
    findOpElse(List(OP_IF,OP_ELSE,OP_ENDIF,OP_ELSE)) must be (Some(1))
    findOpElse(List(OP_0,OP_1,OP_2)) must be (None)
  }

  it must "find the indexes of OP_ELSE and OP_ENDIF in a list of script tokens" in {
    findIndexesOpElseOpEndIf(List(OP_ELSE,OP_ENDIF)) must be (Some(0),Some(1))
    findIndexesOpElseOpEndIf(List(OP_IF, OP_ELSE,OP_ENDIF, OP_IF,OP_ELSE,OP_ENDIF)) must be (Some(1),Some(2))
    findIndexesOpElseOpEndIf(List(OP_IF,OP_IF)) must be (None,None)
  }


  it must "evaluate an OP_IF correctly" in {
    val stack = List(OP_0)
    val script = List(OP_IF, OP_RESERVED, OP_ENDIF, OP_1)
    val (newStack,newScript) = opIf(stack,script)
    newStack.isEmpty must be (true)
    newScript must be (List(OP_1))
  }


  it must "evaluate an OP_IF OP_ELSE OP_ENDIF block" in {
    val stack = List(OP_0)
    val script = List(OP_IF, OP_VER, OP_ELSE, OP_1, OP_ENDIF)
    val (newStack,newScript) = opIf(stack,script)
    newScript must be (List(OP_1,OP_ENDIF))
  }

  it must "evaluate an OP_IF block correctly if the stack top is true" in {
    val stack = List(OP_1)
    val script = List(OP_IF, OP_1, OP_ELSE, OP_0, OP_ENDIF)
    val (newStack,newScript) = opIf(stack,script)

    newStack must be (List())
    newScript must be (List(OP_1, OP_ENDIF))
  }

  it must "evaluate a weird case using multiple OP_ELSEs" in {
    val stack = List(ScriptNumberImpl(1))
    val script = List(OP_IF, OP_ELSE, OP_0, OP_ELSE, OP_1, OP_ENDIF)

    val (newStack,newScript) = opIf(stack,script)

    newScript must be (List(OP_ELSE,OP_1,OP_ENDIF))
  }
}
