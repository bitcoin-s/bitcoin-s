package org.bitcoins.core.protocol

import org.bitcoins.core.config.{ MainNet, TestNet3 }
import org.scalatest.{ FlatSpec, MustMatchers }

class HumanReadablePartTest extends FlatSpec with MustMatchers {

  "HumanReadablePart" must "match the correct hrp with the correct string" in {
    HumanReadablePart("tb") must be(tb)
    HumanReadablePart("bc") must be(bc)
  }

  it must "match the correct hrp with the correct network" in {
    HumanReadablePart(TestNet3) must be(tb)
    HumanReadablePart(MainNet) must be(bc)
  }
}
