package org.bitcoins.core.api.wallet

import scala.concurrent.Future

trait RescanHandlingApi {
  def isRescanning(): Future[Boolean]
}
