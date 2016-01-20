package org.scalacoin.script.stack

import org.scalacoin.script.constant.ScriptConstantImpl
import org.scalacoin.util.ScalacoinUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/6/16.
 */
class StackInterpreterTest extends FlatSpec with MustMatchers with StackInterpreter {
  val stack = List(ScriptConstantImpl("Hello"),ScriptConstantImpl("World"))
  "StackInterpreter" must "duplicate elements on top of the stack" in {

    val script = List(OP_DUP)
    val (newStack,newScript) = opDup(stack,script)

    newStack.head must be (ScriptConstantImpl("Hello"))
    newStack(1) must be (ScriptConstantImpl("Hello"))
    newStack(2) must be (ScriptConstantImpl("World"))
  }

  it must "throw an exception when calling opDup without an OP_DUP on top of the script stack" in {

    intercept[IllegalArgumentException] {
      val script = List()
      val (newStack,newScript) = opDup(stack,script)
    }
  }

  it must "throw an exception when calling opDup without an element on top of the stack" in {

    intercept[IllegalArgumentException] {
      val stack = List()
      val script = List(OP_DUP)
      val (newStack,newScript) = opDup(stack,script)
    }
  }

  it must "evaluate the OP_DEPTH operator correctly" in {
    val stack = List(ScriptConstantImpl("Hello"),ScriptConstantImpl("World"))
    val script = List(OP_DEPTH)
    val (newStack,newScript) = opDepth(stack,script)

    newStack.head.hex must be (ScalacoinUtil.encodeHex(stack.size.toByte))
  }

  it must "evaluate OP_DEPTH operator correctly when there are zero items on the stack" in {
    val stack = List()
    val script = List(OP_DEPTH)
    val (newStack,_) = opDepth(stack,script)
    newStack.head.hex must be (stack.size.toHexString+"0")
  }
}
