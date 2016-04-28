package org.bitcoins.script.stack

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/8/16.
 */
class StackOperationFactoryTest extends FlatSpec with MustMatchers {

  "StackOperationFactory" must "match correct operations with their strings" in {
    StackOperation.fromString("OP_DUP") must be (Some(OP_DUP))
    StackOperation.fromString("OP_FROMALTSTACK") must be (Some(OP_FROMALTSTACK))
    StackOperation.fromString("RANDOM_OP") must be (None)
    StackOperation.fromString("OP_IFDUP") must be (Some(OP_IFDUP))
  }
}
