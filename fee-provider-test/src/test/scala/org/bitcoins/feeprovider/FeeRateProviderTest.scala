package org.bitcoins.feeprovider

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.config._
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.feeprovider.MempoolSpaceTarget._
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalatest.Assertion

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

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

  it must "get a valid fee rate from mempool.space using the fastest fee target" in {
    val provider = MempoolSpaceProvider(FastestFeeTarget, MainNet)
    testProvider(provider)
  }

  it must "get a valid fee rate from mempool.space using a half hour fee target" in {
    val provider = MempoolSpaceProvider(HalfHourFeeTarget, MainNet)
    testProvider(provider)
  }

  it must "get a valid fee rate from mempool.space using an hour fee target" in {
    val provider = MempoolSpaceProvider(HourFeeTarget, MainNet)
    testProvider(provider)
  }

  it must "get a valid fee rate from mempool.space/testnet using the fastest fee target" in {
    val provider = MempoolSpaceProvider(FastestFeeTarget, TestNet3)
    testProvider(provider)
  }

  it must "get a valid fee rate from mempool.space/testnet using a half hour fee target" in {
    val provider = MempoolSpaceProvider(HalfHourFeeTarget, TestNet3)
    testProvider(provider)
  }

  it must "get a valid fee rate from mempool.space/testnet using an hour fee target" in {
    val provider = MempoolSpaceProvider(HourFeeTarget, TestNet3)
    testProvider(provider)
  }

  it must "get a valid fee rate from mempool.space/signet using the fastest fee target" in {
    val provider = MempoolSpaceProvider(FastestFeeTarget, SigNet)
    testProvider(provider)
  }

  it must "get a valid fee rate from mempool.space/signet using a half hour fee target" in {
    val provider = MempoolSpaceProvider(HalfHourFeeTarget, SigNet)
    testProvider(provider)
  }

  it must "get a valid fee rate from mempool.space/signet using an hour fee target" in {
    val provider = MempoolSpaceProvider(HourFeeTarget, SigNet)
    testProvider(provider)
  }

  it must "get a cached fee rate from a cachedHttpFeeRateProvider" in {
    val provider = MempoolSpaceProvider(FastestFeeTarget, MainNet)
    for {
      feeRate <- provider.getFeeRate
      _ <- AsyncUtil.nonBlockingSleep(20.seconds)
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
