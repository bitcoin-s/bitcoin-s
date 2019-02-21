package org.bitcoins.node.versions

import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 6/6/16.
  */
class ProtocolVersionTest extends FlatSpec with MustMatchers {

  "ProtocolVersion" must "give us the correct protocol version back from its hex format" in {
    ProtocolVersion("72110100") must be(ProtocolVersion70002)
  }
}
