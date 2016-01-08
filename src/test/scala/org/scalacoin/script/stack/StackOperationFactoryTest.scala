package org.scalacoin.script.stack

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/8/16.
 */
class StackOperationFactoryTest extends FlatSpec with MustMatchers with StackOperationFactory {

  "StackOperationFactory" must "match correct operations with their strings" in {
    fromString("OP_DUP") must be (Some(OP_DUP))
    fromString("OP_FROMALTSTACK") must be (Some(OP_FROMALTSTACK))
    fromString("RANDOM_OP") must be (None)
    fromString("OP_IFDUP") must be (Some(OP_IFDUP))
  }
}
