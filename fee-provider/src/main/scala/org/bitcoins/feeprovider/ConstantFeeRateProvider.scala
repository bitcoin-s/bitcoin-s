package org.bitcoins.feeprovider

import org.bitcoins.core.api.FeeRateApi
import org.bitcoins.core.wallet.fee.FeeUnit

import scala.concurrent.Future

case class ConstantFeeRateProvider(feeUnit: FeeUnit) extends FeeRateApi {
  def getFeeRate: Future[FeeUnit] = Future.successful(feeUnit)
}
