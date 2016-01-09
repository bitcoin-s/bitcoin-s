package org.scalacoin.script.control

import org.scalacoin.script.constant.ScriptConstantImpl
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/6/16.
 */
class ControlOperationsInterpreterTest extends FlatSpec with MustMatchers with ControlOperationsInterpreter {

  "ControlOperationsInterpreter" must "have OP_VERIFY evaluate to true with '1' on the stack" in {
    val stack = List(ScriptConstantImpl("1"))
    val script = List(OP_VERIFY)
    val result = verify(stack,script)
    result must be (true)
  }

  it must "have OP_VERIFY evaluate to false with '0' on the stack" in {
    val stack = List(ScriptConstantImpl("0"))
    val script = List(OP_VERIFY)
    val result = verify(stack,script)
    result must be (false)
  }

  it must "have OP_VERIFY evaluate to false with '2' on the stack" in {
    val stack = List(ScriptConstantImpl("2"))
    val script = List(OP_VERIFY)
    val result = verify(stack,script)
    result must be (false)
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
}
