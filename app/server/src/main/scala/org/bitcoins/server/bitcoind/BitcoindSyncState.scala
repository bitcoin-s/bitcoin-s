package org.bitcoins.server.bitcoind

import org.bitcoins.server.util.BitcoindPollingCancellabe

import scala.concurrent.Future

/** @param syncF the future that will be completed when the synchronization with bitcoind is complete
  * @param pollingCancellable You can cancel bitcoind polling by calling [[BitcoindPollingCancellabe.cancel()]]
  */
case class BitcoindSyncState(
    syncF: Future[Unit],
    pollingCancellable: BitcoindPollingCancellabe) {

  /** Stops syncing and polling bitcoind */
  def stop(): Future[Unit] = {
    pollingCancellable.cancel()
    syncF
  }
}
