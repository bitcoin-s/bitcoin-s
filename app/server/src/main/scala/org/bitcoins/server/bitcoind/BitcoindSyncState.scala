package org.bitcoins.server.bitcoind

import org.bitcoins.server.util.{BitcoindPollingCancellable}

import scala.concurrent.Future

/** @param initSyncF
  *   the future that will be completed when the initial synchronization with
  *   bitcoind is complete. This Future isn't related to subsequent polling jobs
  *   after our initial sync between bitcoind and the wallet on startup
  * @param pollingCancellable
  *   You can cancel bitcoind polling by calling
  *   [[BitcoindPollingCancellabe.cancel()]]
  */
case class BitcoindSyncState(
    initSyncF: Future[Unit],
    pollingCancellable: BitcoindPollingCancellable
) {

  /** Stops syncing and polling bitcoind */
  def stop(): Future[Unit] = {
    pollingCancellable.cancel()
    initSyncF
  }
}
