package org.scalacoin.script.splice

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/22/16.
 */
class SpliceOperationFactoryTest extends FlatSpec with MustMatchers {

  "SpliceOperationFactory" must "instantiate the splice operations from hex" in {
    SpliceOperationsFactory.fromHex("7e") must be (Some(OP_CAT))
    SpliceOperationsFactory.fromHex("7f") must be (Some(OP_SUBSTR))
  }

  it must "instantiate splice operations from their byte values" in {
    SpliceOperationsFactory.fromByte(126.toByte) must be (Some(OP_CAT))
    SpliceOperationsFactory.fromByte(127.toByte) must be (Some(OP_SUBSTR))
  }
}
