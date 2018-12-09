package org.bitcoins.core.script.locktime

import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 1/8/16.
  */
class LocktimeOperationFactoryTest extends FlatSpec with MustMatchers {

  "LocktimeOperationFactory" must "match lock time operations from strings" in {
    LocktimeOperation.fromString("OP_CHECKLOCKTIMEVERIFY") must be(
      Some(OP_CHECKLOCKTIMEVERIFY))
  }

}
