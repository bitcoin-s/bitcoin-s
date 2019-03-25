package org.bitcoins.wallet.api

import org.bitcoins.core.wallet.fee.FeeUnit

import scala.concurrent.Future

trait FeeProvider {

  /**
    * @param confirmationTarget confirmation target in number
    *                           of blocks
    * @return the estimated fee level needed for the given
    *         number of blocks
    */
  def getFee(confirmationTarget: Int): Future[FeeUnit]
}
