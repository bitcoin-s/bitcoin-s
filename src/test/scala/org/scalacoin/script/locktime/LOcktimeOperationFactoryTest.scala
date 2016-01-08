package org.scalacoin.script.locktime

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/8/16.
 */
class LocktimeOperationFactoryTest extends FlatSpec with MustMatchers with LocktimeOperationFactory {

  "LocktimeOperationFactory" must "match lock time operations from strings" in {
    fromString("OP_CHECKLOCKTIMEVERIFY") must be (Some(OP_CHECKLOCKTIMEVERIFY))
  }

}
