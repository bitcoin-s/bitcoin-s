package org.bitcoins.feeprovider

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class FeeRateProviderTest extends BitcoinSAsyncTest {

  it must "get a valid fee rate from bitcoiner.live" in {
    val provider = BitcoinerLiveFeeRateProvider(60)
    provider.getFeeRate.map { feeRate =>
      assert(feeRate.toLong > 0)
    }
  }

  it must "get a cached fee rate from a cachedHttpFeeRateProvider" in {
    val provider = BitcoinerLiveFeeRateProvider(60)
    for {
      feeRate <- provider.getFeeRate
      cached <- provider.getFeeRate
    } yield assert(feeRate == cached)
  }

  it must "fail to create a BitcoinerLiveFeeRateProvider with invalid minutes" in {
    assertThrows[IllegalArgumentException](BitcoinerLiveFeeRateProvider(-1))
  }

  it must "get the correct fee rate from a ConstantFeeRateProvider" in {
    val provider = ConstantFeeRateProvider(SatoshisPerByte(Satoshis(4)))
    provider.getFeeRate.map { feeRate =>
      assert(feeRate == SatoshisPerByte(Satoshis(4)))
    }
  }
}
