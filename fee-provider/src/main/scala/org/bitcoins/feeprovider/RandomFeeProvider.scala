package org.bitcoins.feeprovider

import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}

import scala.concurrent.Future

/** Generates a uniformly distributed random fee between [1-10,000]
  * satoshis/vbyte This should not be used in production.
  */
class RandomFeeProvider extends FeeRateApi {
  // Useful for tests
  var lastFeeRate: Option[FeeUnit] = None

  override def getFeeRate(): Future[FeeUnit] = {
    val raw = scala.util.Random.between(1, 10000)
    val feeRate = SatoshisPerVirtualByte(Satoshis(raw))
    lastFeeRate = Some(feeRate)
    Future.successful(feeRate)
  }
}
