package org.bitcoins.core.wallet.fee

import org.bitcoins.testkit.util.BitcoinSUnitTest

class FeeUnitTest extends BitcoinSUnitTest {

  "SatoshisPerByte" must "have the correct FeeUnit" in {
    assert(SatoshisPerByte.one.units == FeeUnit.PerByte)
  }

  "SatoshisPerVirtualByte" must "have the correct FeeUnit" in {
    assert(SatoshisPerVirtualByte.one.units == FeeUnit.PerVirtualByte)
  }

  "SatoshisPerKiloByte" must "have the correct FeeUnit" in {
    assert(SatoshisPerKiloByte.one.units == FeeUnit.PerKiloByte)
  }

  "BitcoinsPerKiloByte" must "have the correct FeeUnit" in {
    assert(BitcoinsPerKiloByte.one.units == FeeUnit.PerKiloByte)
  }

  "FlatSatoshis" must "have the correct FeeUnit" in {
    assert(FlatSatoshis(1000).units == FeeUnit.Flat)
  }
}
