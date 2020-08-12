package org.bitcoins.core.api.feeprovider

import org.bitcoins.core.wallet.fee.FeeUnit

import scala.concurrent.Future

trait FeeRateApi {

  def getFeeRate: Future[FeeUnit]

}
