package org.scalacoin.script.bitwise

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/8/16.
 */
class BitwiseOperationsFactoryTest extends FlatSpec with MustMatchers with BitwiseOperationsFactory {

  "BitwiseOperationsFactory" must "match strings with bitwise operations" in {
    fromString("OP_EQUAL") must be (Some(OP_EQUAL))
    fromString("OP_EQUALVERIFY") must be (Some(OP_EQUALVERIFY))
    fromString("RANDOM") must be (None)
  }
}
