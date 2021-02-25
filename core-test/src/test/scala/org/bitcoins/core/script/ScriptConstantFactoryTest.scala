package org.bitcoins.core.script

import org.bitcoins.core.script.constant.ScriptConstant
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 4/1/16.
  */
class ScriptConstantFactoryTest extends BitcoinSUnitTest {

  "ScriptConstantFactory" must "create a constant from bytes" in {
    val bytes = BytesUtil.decodeHex("abc123")
    ScriptConstant(bytes).bytes must be(bytes)
  }

}
