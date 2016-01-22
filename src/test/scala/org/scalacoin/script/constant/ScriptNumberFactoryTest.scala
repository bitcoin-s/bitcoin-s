package org.scalacoin.script.constant

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/9/16.
 */
class ScriptNumberFactoryTest extends FlatSpec with MustMatchers with ScriptNumberFactory {


  "ScriptNumberFactory" must "represent the number 1" in  {
    operations.exists(_ == ScriptNumberImpl(1)) must be (true)
    operations.exists(_ == ScriptNumberImpl(75)) must be (true)
  }

  it must "find the number two" in {
    val result = ScriptNumberFactory.factory(2)
    result.isDefined must be (true)
    result.get.opCode must be (2)
    result.get must be (ScriptNumberImpl(2))
  }

  it must "find the number two from its byte representation" in {
    val result = ScriptNumberFactory.factory(0x02)
    result.isDefined must be (true)
    result.get.opCode must be (2)
    result.get must be (ScriptNumberImpl(2))
  }

  it must "not allow creation of the script number -2" in {
    intercept[IllegalArgumentException] {
      operations.exists(_ == ScriptNumberImpl(-2)) must be (false)
    }
  }

  it must "not allow creation of the script number 76" in {
    intercept[IllegalArgumentException] {
      operations.exists(_ == ScriptNumberImpl(76)) must be (false)
    }
  }
}
