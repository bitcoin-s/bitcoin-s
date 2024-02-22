package org.bitcoins.server.util

import grizzled.slf4j.Logging
import org.apache.pekko.actor.Cancellable

case class BitcoindPollingCancellable(
    blockPollingCancellable: Cancellable,
    mempoolPollingCancelable: Cancellable)
    extends Cancellable
    with Logging {

  override def cancel(): Boolean = {
    logger.info(s"Cancelling bitcoind polling jobs")
    blockPollingCancellable.cancel() && mempoolPollingCancelable.cancel()
  }

  override def isCancelled: Boolean =
    blockPollingCancellable.isCancelled && mempoolPollingCancelable.cancel()
}

object BitcoindPollingCancellabe {

  val none: BitcoindPollingCancellable = BitcoindPollingCancellable(
    Cancellable.alreadyCancelled,
    Cancellable.alreadyCancelled)
}
