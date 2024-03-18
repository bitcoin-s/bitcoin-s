package org.bitcoins.server.util

import org.apache.pekko.actor.Cancellable
import org.bitcoins.commons.util.BitcoinSLogger

case class BitcoindPollingCancellable(
    blockPollingCancellable: Cancellable,
    mempoolPollingCancelable: Cancellable)
    extends Cancellable
    with BitcoinSLogger {

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
