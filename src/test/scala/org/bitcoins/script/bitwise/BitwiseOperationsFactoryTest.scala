package org.bitcoins.script.bitwise

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/8/16.
 */
class BitwiseOperationsFactoryTest extends FlatSpec with MustMatchers {

  "BitwiseOperationsFactory" must "match strings with bitwise operations" in {
    BitwiseOperation.fromString("OP_EQUAL") must be (Some(OP_EQUAL))
    BitwiseOperation.fromString("OP_EQUALVERIFY") must be (Some(OP_EQUALVERIFY))
    BitwiseOperation.fromString("RANDOM") must be (None)
  }
}
