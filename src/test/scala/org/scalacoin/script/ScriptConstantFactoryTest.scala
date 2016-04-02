package org.scalacoin.script

import org.scalacoin.script.constant.ScriptConstantFactory
import org.scalacoin.util.BitcoinSUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 4/1/16.
 */
class ScriptConstantFactoryTest extends FlatSpec with MustMatchers {

  "ScriptConstantFactory" must "create a constant from bytes" in {
    val bytes = BitcoinSUtil.decodeHex("abc123")
    ScriptConstantFactory.fromBytes(bytes).bytes must be (bytes)
  }

}
