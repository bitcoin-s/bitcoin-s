package org.bitcoins.core.api.keymanager

import scala.concurrent.{ExecutionContext, Future}

trait MasterXPubApi {

  /** Determines if the seed exists */
  def seedExists()(implicit ec: ExecutionContext): Future[Boolean]
}
