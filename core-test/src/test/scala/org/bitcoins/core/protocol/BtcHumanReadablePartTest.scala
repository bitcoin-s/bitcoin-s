package org.bitcoins.core.protocol

import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}
import org.scalatest.{FlatSpec, MustMatchers}

import scala.util.Success

class BtcHumanReadablePartTest extends FlatSpec with MustMatchers {
  import BtcHumanReadablePart._

  "HumanReadablePart" must "match the correct hrp with the correct string" in {
    BtcHumanReadablePart("tb") must be(Success(tb))
    BtcHumanReadablePart("bc") must be(Success(bc))
    BtcHumanReadablePart("bcrt") must be(Success(bcrt))
  }

  it must "match the correct hrp with the correct network" in {
    BtcHumanReadablePart(TestNet3) must be(tb)
    BtcHumanReadablePart(MainNet) must be(bc)
    BtcHumanReadablePart(RegTest) must be(bcrt)
  }
}
