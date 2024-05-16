package org.bitcoins.core.api.mempool

import org.bitcoins.core.protocol.transaction.Transaction

import scala.concurrent.Future

trait MempoolApi {
  def acceptToMemoryPool(tx: Transaction): Future[MempoolAcceptResult]
}
