package org.bitcoins.core.script.constant

import org.bitcoins.crypto.BytesUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 4/4/16.
  */
class ScriptNumberFactoryTest extends BitcoinSUnitTest {

  "ScriptNumber" must "create the same number using from hex and from bytes" in {
    val hex = "ff7f"
    val bytes = BytesUtil.decodeHex(hex)
    ScriptNumber(hex) must be(ScriptNumber(bytes))
  }

  it must "create the same number using fromNumber & fromHex if no extra padding is used" in {
    val hex = "ff7f"
    val number = 32767
    ScriptNumber(number) must be(ScriptNumber(hex))
  }

  it must "create a negative number from hex" in {
    val hex = "008080"
    val number = -32768
    ScriptNumber(hex).toLong must be(number)
  }

  it must "create a negative number from hex and have the same underlying hex representation" in {
    val hex = "008080"
    ScriptNumber(hex).hex must be(hex)
  }

  it must "create negative number using fromNumber and get the right hex" in {
    val hex = "008080"
    val number = -32768
    ScriptNumber(number).hex must be(hex)
  }

  it must "it must give us the same object when we create the number zero from the ScriptNumber" in {
    ScriptNumber(0) must be(ScriptNumber.zero)
  }

  it must "convert our script number 0 to a long" in {
    ScriptNumber.zero.toLong must be(0)
  }
}
