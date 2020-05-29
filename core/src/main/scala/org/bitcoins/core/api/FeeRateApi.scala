package org.bitcoins.core.api

import org.bitcoins.core.wallet.fee.FeeUnit

import scala.concurrent.Future

abstract class FeeRateApi {

  def getFeeRate: Future[FeeUnit]

}
