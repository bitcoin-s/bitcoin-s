package org.bitcoins.core.script

import org.bitcoins.core.script.constant.ScriptConstant
import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{ FlatSpec, MustMatchers }

/**
 * Created by chris on 4/1/16.
 */
class ScriptConstantFactoryTest extends FlatSpec with MustMatchers {

  "ScriptConstantFactory" must "create a constant from bytes" in {
    val bytes = BitcoinSUtil.decodeHex("abc123")
    ScriptConstant(bytes).bytes must be(bytes)
  }

}
