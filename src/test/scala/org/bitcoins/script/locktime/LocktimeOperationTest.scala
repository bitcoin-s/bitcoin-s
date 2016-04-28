package org.bitcoins.script.locktime

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/6/16.
 */
class LocktimeOperationTest extends FlatSpec with MustMatchers {

  "LocktimeOperations" must "define OP_CHECKLOCKTIMEVERIFY" in {
    OP_CHECKLOCKTIMEVERIFY.opCode must be (177)
  }
}
