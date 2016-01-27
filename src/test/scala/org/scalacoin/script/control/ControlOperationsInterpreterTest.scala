package org.scalacoin.script.control

import org.scalacoin.script.arithmetic.OP_ADD
import org.scalacoin.script.bitwise.OP_EQUAL
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

  it must "find the first index of our OP_ENDIF in a list of script tokens" in {
    val l = List(OP_ENDIF)
    findFirstOpEndIf(l) must be (Some(0))
    findFirstOpEndIf(List(OP_IF,OP_ELSE,OP_ENDIF,OP_ENDIF)) must be (Some(2))
    findFirstOpEndIf(List(OP_0,OP_1,OP_2)) must be (None)
    findFirstOpEndIf(List(OP_IF, OP_RESERVED, OP_ENDIF, OP_1)) must be (Some(2))

  }

  it must "find the last index of our OP_ENDIF in a list of script tokens" in {
    val l = List(OP_ENDIF)
    findLastOpEndIf(l) must be (Some(0))
    findLastOpEndIf(List(OP_IF,OP_ELSE,OP_ENDIF,OP_ENDIF)) must be (Some(3))
    findLastOpEndIf(List(OP_0,OP_1,OP_2)) must be (None)
    findLastOpEndIf(List(OP_IF, OP_RESERVED, OP_ENDIF, OP_ENDIF, OP_1)) must be (Some(3))

  }

  it must "find the first indexes of OP_ELSE in a list of script tokens" in {
    findFirstOpElse(List(OP_ELSE)) must be (Some(0))
    findFirstOpElse(List(OP_IF,OP_ELSE,OP_ENDIF,OP_ELSE)) must be (Some(1))
    findFirstOpElse(List(OP_0,OP_1,OP_2)) must be (None)
  }

  it must "find the first indexes of OP_ELSE and OP_ENDIF in a list of script tokens" in {
    findFirstIndexesOpElseOpEndIf(List(OP_ELSE,OP_ENDIF)) must be (Some(0),Some(1))
    findFirstIndexesOpElseOpEndIf(List(OP_IF, OP_ELSE,OP_ENDIF, OP_IF,OP_ELSE,OP_ENDIF)) must be (Some(1),Some(2))
    findFirstIndexesOpElseOpEndIf(List(OP_IF,OP_IF)) must be (None,None)
  }

  it must "remvoe the first OP_IF expression in a script" in {
    removeFirstOpIf(List(OP_IF,OP_ELSE,OP_ENDIF)) must be (List(OP_ELSE,OP_ENDIF))
    removeFirstOpIf(List(OP_ELSE,OP_ENDIF)) must be (List(OP_ELSE,OP_ENDIF))
    removeFirstOpIf(List(OP_IF, OP_1,OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)) must be (List(OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF))
    removeFirstOpIf(List(OP_IF,OP_ENDIF)) must be (List(OP_ENDIF))
  }

  it must "remove the first OP_ELSE expression in a script" in {
    removeFirstOpElse(List(OP_IF,OP_ELSE,OP_ENDIF)) must be (List(OP_IF,OP_ENDIF))
    removeFirstOpElse(List(OP_IF,OP_ENDIF)) must be (List(OP_IF,OP_ENDIF))
    removeFirstOpElse(List(OP_IF, OP_1,OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)) must be (List(OP_IF, OP_1, OP_ELSE, OP_3, OP_ENDIF))
  }

  it must "remove the last OP_ELSE expression in a script" in {
    val script = List(OP_IF, OP_IF, OP_1, OP_ELSE, OP_0, OP_ENDIF, OP_ELSE, OP_IF, OP_0, OP_ELSE, OP_1, OP_ENDIF, OP_ENDIF)

    removeCorrespondingOpElse(script) must be (List(OP_IF, OP_IF, OP_1, OP_ELSE, OP_0, OP_ENDIF, OP_ENDIF))
  }

  it must "find a matching OP_ENDIF for an OP_IF" in {
    //https://gist.github.com/Christewart/381dc1dbbb07e62501c3
    val script = List(OP_IF, OP_1, OP_IF, OP_RETURN, OP_ELSE, OP_RETURN, OP_ELSE, OP_RETURN, OP_ENDIF,
      OP_ELSE, OP_1, OP_IF, OP_1, OP_ELSE, OP_RETURN, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_RETURN, OP_ENDIF, OP_ADD, OP_2, OP_EQUAL)
    findMatchingOpEndIf(script) must be (20)
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
    newScript must be (List(OP_ELSE,OP_1,OP_ENDIF))
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

  it must "evaluate this OP_IF OP_ELSE block correctly" in {
    //https://gist.github.com/Christewart/381dc1dbbb07e62501c3
    val stack = List(OP_0)
    val script = List(OP_IF, OP_1, OP_IF, OP_RETURN, OP_ELSE, OP_RETURN, OP_ELSE, OP_RETURN, OP_ENDIF,
      OP_ELSE, OP_1, OP_IF, OP_1, OP_ELSE, OP_RETURN, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_RETURN, OP_ENDIF, OP_ADD, OP_2, OP_EQUAL)
    val (newStack,newScript) = opIf(stack,script)

    newStack.isEmpty must be (true)
    newScript must be (List(OP_ADD, OP_2, OP_EQUAL))
  }


  it must "evalute nested OP_IFS correctly" in {
    val stack = List(OP_1,OP_1)
    val script = List(OP_IF, OP_IF, OP_1, OP_ELSE, OP_0, OP_ENDIF, OP_ELSE, OP_IF, OP_0, OP_ELSE, OP_1, OP_ENDIF, OP_ENDIF)
    val (newStack,newScript) = opIf(stack,script)

    newStack.head must be (OP_1)
    newScript must be (List(OP_IF,OP_1,OP_ELSE,OP_0,OP_ENDIF,OP_ENDIF))
  }
}


