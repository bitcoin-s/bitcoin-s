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

  it must "not allow creation of the script number 0" in {
    intercept[IllegalArgumentException] {
      operations.exists(_ == ScriptNumberImpl(0)) must be (false)
    }
  }

  it must "not allow creation of the script number 76" in {
    intercept[IllegalArgumentException] {
      operations.exists(_ == ScriptNumberImpl(76)) must be (false)
    }
  }
}
