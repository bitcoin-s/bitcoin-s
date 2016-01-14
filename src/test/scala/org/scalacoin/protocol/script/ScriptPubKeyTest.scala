package org.scalacoin.protocol.script

import org.scalacoin.util.{ScalacoinUtil, TestUtil}
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/14/16.
 */
class ScriptPubKeyTest extends FlatSpec with MustMatchers {

  "ScriptPubKey" must "calculate the correct size for a scriptPubKey" in {
    val expectedSize = ScalacoinUtil.decodeHex(TestUtil.rawScriptPubKey).size
    TestUtil.scriptPubKey.size must be (expectedSize)
  }

}
