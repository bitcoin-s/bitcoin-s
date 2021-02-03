package org.bitcoins.core.script.control

import org.bitcoins.testkit.util.BitcoinSUnitTest

/** Created by chris on 1/8/16.
  */
class ControlOperationsFactoryTest extends BitcoinSUnitTest {

  "ControlOperationsFactory" must "match a string with a control operation" in {
    ControlOperations.fromStringOpt("OP_ELSE") must be(Some(OP_ELSE))
    ControlOperations.fromStringOpt("OP_ENDIF") must be(Some(OP_ENDIF))
    ControlOperations.fromStringOpt("OP_IF") must be(Some(OP_IF))
    ControlOperations.fromStringOpt("OP_NOTIF") must be(Some(OP_NOTIF))
    ControlOperations.fromStringOpt("OP_RETURN") must be(Some(OP_RETURN))
    ControlOperations.fromStringOpt("OP_VERIFY") must be(Some(OP_VERIFY))
    ControlOperations.fromStringOpt("RANDOM") must be(None)
  }

}
