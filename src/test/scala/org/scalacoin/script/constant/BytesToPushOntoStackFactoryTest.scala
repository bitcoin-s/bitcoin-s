package org.scalacoin.script.constant

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/9/16.
 */
class BytesToPushOntoStackFactoryTest extends FlatSpec with MustMatchers with BytesToPushOntoStackFactory {


  "ScriptNumberFactory" must "represent the number 1" in  {
    operations.exists(_ == BytesToPushOntoStackImpl(1)) must be (true)
    operations.exists(_ == BytesToPushOntoStackImpl(75)) must be (true)
  }

  it must "find the number two" in {
    val result = BytesToPushOntoStackFactory.factory(2)
    result.isDefined must be (true)
    result.get.opCode must be (2)
    result.get must be (BytesToPushOntoStackImpl(2))
  }

  it must "find the number two from its byte representation" in {
    val result = BytesToPushOntoStackFactory.factory(0x02)
    result.isDefined must be (true)
    result.get.opCode must be (2)
    result.get must be (BytesToPushOntoStackImpl(2))
  }

  it must "not allow creation of the script number -2" in {
    intercept[IllegalArgumentException] {
      operations.exists(_ == BytesToPushOntoStackImpl(-2)) must be (false)
    }
  }

}
