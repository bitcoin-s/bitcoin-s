package org.scalacoin.script.control

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/8/16.
 */
class ControlOperationsFactoryTest extends FlatSpec with MustMatchers with ControlOperationsFactory {

  "ControlOperationsFactory" must "match a string with a control operation" in {
    fromString("OP_ELSE") must be (Some(OP_ELSE))
    fromString("OP_ENDIF") must be (Some(OP_ENDIF))
    fromString("OP_IF") must be (Some(OP_IF))
    fromString("OP_NOP") must be (Some(OP_NOP))
    fromString("OP_NOTIF") must be (Some(OP_NOTIF))
    fromString("OP_RETURN") must be (Some(OP_RETURN))
    fromString("OP_VERIFY") must be (Some(OP_VERIFY))
    fromString("RANDOM") must be (None)
  }

}
