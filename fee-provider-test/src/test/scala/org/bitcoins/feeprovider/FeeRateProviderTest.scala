package org.bitcoins.feeprovider

import org.bitcoins.core.api.FeeRateApi
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalatest.Assertion

import scala.concurrent.Future

class FeeRateProviderTest extends BitcoinSAsyncTest {

  it must "get a valid fee rate from bitcoiner.live" in {
    val provider = BitcoinerLiveFeeRateProvider(60)
    testProvider(provider)
  }

  it must "get a valid fee rate from BitGo without a block target" in {
    val provider = BitGoFeeRateProvider(None)
    testProvider(provider)
  }

  it must "get a valid fee rate from BitGo with a block target" in {
    val provider = BitGoFeeRateProvider(Some(100))
    testProvider(provider)
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

  private def testProvider(provider: FeeRateApi): Future[Assertion] = {
    provider.getFeeRate.map { feeRate =>
      assert(feeRate.toLong > 0)
    }
  }
}
