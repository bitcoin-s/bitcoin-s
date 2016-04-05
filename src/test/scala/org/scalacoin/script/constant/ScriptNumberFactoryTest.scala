package org.scalacoin.script.constant

import org.scalacoin.util.BitcoinSUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 4/4/16.
 */
class ScriptNumberFactoryTest extends FlatSpec with MustMatchers {

  "ScriptNumberFactory" must "create the same number using from hex and from bytes" in {
    val hex = "ff7f"
    val bytes = BitcoinSUtil.decodeHex(hex)
    ScriptNumberFactory.fromHex(hex) must be (ScriptNumberFactory.fromBytes(bytes))
  }

  it must "create the same number using fromNumber & fromHex if no extra padding is used" in {
    val hex = "ff7f"
    val number = 32767
    ScriptNumberFactory.fromNumber(number) must be (ScriptNumberFactory.fromHex(hex))
  }

  it must "create a negative number from hex" in {
    val hex = "008080"
    val number = -32768
    ScriptNumberFactory.fromHex(hex).num must be (number)
  }

  it must "create a negative number from hex and have the same underlying hex representation" in {
    val hex = "008080"
    ScriptNumberFactory.fromHex(hex).hex must be (hex)
  }

  it must "create negative number using fromNumber and get the right hex" in {
    val hex = "008080"
    val number = -32768
    ScriptNumberFactory.fromNumber(number).hex must be (hex)
  }
}
