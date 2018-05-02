package org.bitcoins.core.script.constant

import org.scalatest.{ MustMatchers, FlatSpec }

/**
 * Created by chris on 1/9/16.
 */
class BytesToPushOntoStackFactoryTest extends FlatSpec with MustMatchers {

  "ScriptNumberFactory" must "represent the number 1" in {
    BytesToPushOntoStack.operations.exists(_ == BytesToPushOntoStack(1)) must be(true)
    BytesToPushOntoStack.operations.exists(_ == BytesToPushOntoStack(75)) must be(true)
  }

  it must "find the number two" in {
    val result = BytesToPushOntoStack(2)

    result.opCode must be(2)
    result must be(BytesToPushOntoStack(2))
  }

  it must "find the number two from its byte representation" in {
    val result = BytesToPushOntoStack(0x02)

    result.opCode must be(2)
    result must be(BytesToPushOntoStack(2))
  }

  it must "not allow creation of the script number -2" in {
    intercept[IllegalArgumentException] {
      BytesToPushOntoStack.operations.exists(_ == BytesToPushOntoStack(-2)) must be(false)
    }
  }

}
