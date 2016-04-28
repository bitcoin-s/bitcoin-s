package org.bitcoins.script.reserved

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/22/16.
 */
class ReservedOperationsFactoryTest extends FlatSpec with MustMatchers {

  "ReservedOperationsFactory" must "instantiate reserved operations" in {
    ReservedOperation("50") must be (Some(OP_RESERVED))
    ReservedOperation("62") must be (Some(OP_VER))
  }

  it must "find OP_NOP1 from its hex value" in {
    ReservedOperation("b0") must be (Some(OP_NOP1))
  }
  it must "find an undefined operation from its hex value" in  {
    ReservedOperation("ba").isDefined must be (true)
  }
}
