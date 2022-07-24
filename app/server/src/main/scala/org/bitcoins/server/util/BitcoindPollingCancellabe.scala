package org.bitcoins.server.util

import akka.actor.Cancellable

case class BitcoindPollingCancellabe(
    blockPollingCancellable: Cancellable,
    mempoolPollingCancelable: Cancellable)
    extends Cancellable {

  override def cancel(): Boolean =
    blockPollingCancellable.cancel() && mempoolPollingCancelable.cancel()

  override def isCancelled: Boolean =
    blockPollingCancellable.isCancelled && mempoolPollingCancelable.cancel()
}

object BitcoindPollingCancellabe {

  val none: BitcoindPollingCancellabe = BitcoindPollingCancellabe(
    Cancellable.alreadyCancelled,
    Cancellable.alreadyCancelled)
}
