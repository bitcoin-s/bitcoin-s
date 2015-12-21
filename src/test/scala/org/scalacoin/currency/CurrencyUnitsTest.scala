package org.scalacoin.currency


import org.scalatest.Matchers
import org.scalatest.FlatSpec
/**
 * Created by chris on 12/21/15.
 */


class CurrencyUnitsTest extends FlatSpec with Matchers {

  "One satoshi" should ("be equivalent to  0.00000001 BTC") in {
    CurrencyUnits.sataoshisToBitcoin(Satoshis(1)).value shouldBe (0.00000001)
  }

  "One satoshi" should ("be equivalent to 0.001 bits") in {
    CurrencyUnits.satoshisToBits(Satoshis(1)).value shouldBe (0.01)
  }

  "One bit" should ("be equivalent to 100 Satoshis") in {
    CurrencyUnits.bitsToSatoshis(Bits(1)).value shouldBe (100)
  }

  "One bit" should ("be equivalent to 0.000001 BTC") in {
    CurrencyUnits.bitsToBitcoins(Bits(1)).value shouldBe (0.000001)
  }

  "One bitcoin" should ("be equivalent to 100,000,000 Satoshis") in {
    CurrencyUnits.bitcoinsToSatoshis(Bitcoins(1)).value shouldBe (100000000)
  }

  "One bitcoin" should ("be equivalent to 1,000,000 bits") in {
    CurrencyUnits.bitcoinsToBits(Bitcoins(1)).value shouldBe (1000000)
  }

  "The conversion of 1 BTC -> Bits -> Satoshis" should ("equivalent to BTC -> Satoshis") in {
    val btcToBitsToSatoshis = CurrencyUnits.bitsToSatoshis(CurrencyUnits.bitcoinsToBits(Bitcoins(1)))
    val btcToSatoshis = CurrencyUnits.bitcoinsToSatoshis(Bitcoins(1))

    btcToBitsToSatoshis.value shouldBe (btcToSatoshis.value)
  }

  "The conversion of 100,000,000 Satoshis -> Bits -> BTC" should ("be equivalent to 100,000,000 Satoshis -> BTC") in {
    val satoshisToBitsToBTC = CurrencyUnits.bitsToBitcoins(CurrencyUnits.satoshisToBits(Satoshis(100000000)))
    val satoshisToBTC = CurrencyUnits.sataoshisToBitcoin(Satoshis(100000000))
    satoshisToBTC.value shouldBe (satoshisToBitsToBTC.value)
  }

  "The conversion of 1,000,000 Bits -> Satoshis -> BTC" should ("be equivalent to 1,000,000 Bits -> BTC") in {
    val bits = Bits(1000000)
    val bitsToSatoshisToBTC = CurrencyUnits.sataoshisToBitcoin(CurrencyUnits.bitsToSatoshis(bits))
    val bitsToBTC = CurrencyUnits.bitsToBitcoins(bits)
    bitsToSatoshisToBTC.value shouldBe (bitsToBTC.value)
  }

  it must "convert bitcoins to satoshies" in {
    val bitcoins = Bitcoins(0.75)
    val expectedValue = Satoshis(75000000)
    CurrencyUnits.bitcoinsToSatoshis(bitcoins) shouldBe (expectedValue)
  }

  it must "convert bits to satoshies" in {
    val bits = Bits(75)
    val expectedValue = Satoshis(7500)
    CurrencyUnits.bitsToSatoshis(bits) shouldBe (expectedValue)
  }

  it must "convert bitcoins to bits" in {
    val bitcoins = Bitcoins(0.75)
    val expectedValue = Bits(750000)
    CurrencyUnits.bitcoinsToBits(bitcoins) shouldBe (expectedValue)
  }

  it must "display bitcoins correctly" in {
    val bitcoins = Bitcoins(1.23423523523526)
    bitcoins.toString shouldBe ("1.23424 BTC")

    val bitcoinsRoundUp = Bitcoins(5.2321223523623)
    bitcoinsRoundUp.toString shouldBe ("5.23213 BTC")

    val roundBitcoins = Bitcoins(2.0)
    roundBitcoins.toString shouldBe("2 BTC")

    val zeroBitcoins = Bitcoins(0)
    zeroBitcoins.toString shouldBe ("0 BTC")
  }

  it must "display satoshis correctly" in {
    val satoshis = Satoshis(1)
    satoshis.toString shouldBe ("1 Satoshis")
  }

  it must "display bits correctly" in {
    val bits = Bits(5.23)
    bits.toString shouldBe ("5.23 Bits")
  }

  it must "display milliBits correctly" in {
    val milliBits = MilliBitcoins(1.232312352)
    milliBits.toString shouldBe ("1.23232 mBTC")
  }

  it must "say that 1 BTC is equal to 100,000,000 satoshis" in {
    val bitcoin = Bitcoins(1)
    val satoshis = Satoshis(100000000)
    (satoshis == bitcoin) shouldBe (true)
  }

  it must "throw a requirement failed exception when instantiating satoshis with a double with decimal points" in {
    val invalidSatoshis = 1.11111
    intercept[IllegalArgumentException] {
      Satoshis(invalidSatoshis)
    }

  }

}