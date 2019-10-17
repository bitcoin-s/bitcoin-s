package org.bitcoins.core.script.control

import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 1/8/16.
  */
class ControlOperationsFactoryTest extends BitcoinSUnitTest {

  "ControlOperationsFactory" must "match a string with a control operation" in {
    ControlOperations.fromString("OP_ELSE") must be(Some(OP_ELSE))
    ControlOperations.fromString("OP_ENDIF") must be(Some(OP_ENDIF))
    ControlOperations.fromString("OP_IF") must be(Some(OP_IF))
    ControlOperations.fromString("OP_NOTIF") must be(Some(OP_NOTIF))
    ControlOperations.fromString("OP_RETURN") must be(Some(OP_RETURN))
    ControlOperations.fromString("OP_VERIFY") must be(Some(OP_VERIFY))
    ControlOperations.fromString("RANDOM") must be(None)
  }

}
