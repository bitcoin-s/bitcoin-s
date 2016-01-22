package org.scalacoin.script.reserved

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/22/16.
 */
class ReservedOperationsFactoryTest extends FlatSpec with MustMatchers {

  "ReservedOperationsFactory" must "instantiate reserved operations" in {
    ReservedOperationFactory.fromHex("50") must be (Some(OP_RESERVED))
    ReservedOperationFactory.fromHex("62") must be (Some(OP_VER))
  }
}
