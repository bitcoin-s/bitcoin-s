package org.bitcoins.core.script.splice

import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 1/22/16.
  */
class SpliceOperationFactoryTest extends BitcoinSUnitTest {

  "SpliceOperationFactory" must "instantiate the splice operations from hex" in {
    SpliceOperation("7e") must be(Some(OP_CAT))
    SpliceOperation("7f") must be(Some(OP_SUBSTR))
  }

  it must "instantiate splice operations from their byte values" in {
    SpliceOperation(126.toByte) must be(OP_CAT)
    SpliceOperation(127.toByte) must be(OP_SUBSTR)
  }
}
