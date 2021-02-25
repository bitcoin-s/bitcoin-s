package org.bitcoins.core.protocol

import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import scala.util.Success

class BtcHumanReadablePartTest extends BitcoinSUnitTest {
  import BtcHumanReadablePart._

  "HumanReadablePart" must "match the correct hrp with the correct string" in {
    BtcHumanReadablePart.fromStringT("tb") must be(Success(tb))
    BtcHumanReadablePart.fromStringT("bc") must be(Success(bc))
    BtcHumanReadablePart.fromStringT("bcrt") must be(Success(bcrt))
  }

  it must "match the correct hrp with the correct network" in {
    BtcHumanReadablePart(TestNet3) must be(tb)
    BtcHumanReadablePart(MainNet) must be(bc)
    BtcHumanReadablePart(RegTest) must be(bcrt)
  }
}
