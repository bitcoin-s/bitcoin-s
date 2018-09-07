package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config.{ MainNet, RegTest, TestNet3 }
import org.bitcoins.core.protocol.ln.LnParams._
import org.scalatest.{ FlatSpec, MustMatchers }

import scala.util.Try

class LnHumanReadablePartTest extends FlatSpec with MustMatchers {
  it must "match the correct hrp with the correct network" in {
    LnHumanReadablePart(MainNet).get must be(LnHumanReadablePart(LnBitcoinMainNet))
    LnHumanReadablePart(TestNet3).get must be(LnHumanReadablePart(LnBitcoinTestNet))
    LnHumanReadablePart(RegTest).get must be(LnHumanReadablePart(LnBitcoinRegTest))
    LnHumanReadablePart(MainNet, MilliBitcoins(1000)).get must be(LnHumanReadablePart(LnBitcoinMainNet, MilliBitcoins(1000)))
    LnHumanReadablePart(TestNet3, MilliBitcoins(1000)).get must be(LnHumanReadablePart(LnBitcoinTestNet, MilliBitcoins(1000)))
    LnHumanReadablePart(RegTest, MilliBitcoins(1000)).get must be(LnHumanReadablePart(LnBitcoinRegTest, MilliBitcoins(1000)))
  }

  it must "correctly serialize the hrp to string" in {
    LnHumanReadablePart(LnBitcoinMainNet, MilliBitcoins(1000)).toString must be("lnbc1000m")
    LnHumanReadablePart(LnBitcoinTestNet, MilliBitcoins(1000)).toString must be("lntb1000m")
    LnHumanReadablePart(LnBitcoinRegTest, MilliBitcoins(1000)).toString must be("lnbcrt1000m")
    LnHumanReadablePart(LnBitcoinMainNet).toString must be("lnbc")
    LnHumanReadablePart(LnBitcoinTestNet).toString must be("lntb")
    LnHumanReadablePart(LnBitcoinRegTest).toString must be("lnbcrt")
  }

  it must "fail to create hrp from invalid amount" in {
    Try(LnHumanReadablePart(LnBitcoinMainNet, MilliBitcoins(LnPolicy.maxAmountMSat.toBigInt + 1))).isFailure must be(true)
    Try(LnHumanReadablePart(LnBitcoinMainNet, MilliBitcoins(0))).isFailure must be(true)
    Try(LnHumanReadablePart(LnBitcoinMainNet, MilliBitcoins(-1))).isFailure must be(true)
  }

  it must "deserialize hrp from string" in {
    LnHumanReadablePart("lnbc").get must be(LnHumanReadablePart(LnBitcoinMainNet))
    LnHumanReadablePart("lntb").get must be(LnHumanReadablePart(LnBitcoinTestNet))
    LnHumanReadablePart("lnbcrt").get must be(LnHumanReadablePart(LnBitcoinRegTest))
    LnHumanReadablePart("lnbc1000m").get must be(LnHumanReadablePart(LnBitcoinMainNet, MilliBitcoins(1000)))
    LnHumanReadablePart("lntb1000m").get must be(LnHumanReadablePart(LnBitcoinTestNet, MilliBitcoins(1000)))
    LnHumanReadablePart("lnbcrt1000m").get must be(LnHumanReadablePart(LnBitcoinRegTest, MilliBitcoins(1000)))
  }

  it must "fail to deserialize hrp from invalid string" in {
    LnHumanReadablePart("invalid").isFailure must be(true)
    LnHumanReadablePart("lnbc9000").isFailure must be(true)
    LnHumanReadablePart("lnbc90z0m").isFailure must be(true)
  }
}
