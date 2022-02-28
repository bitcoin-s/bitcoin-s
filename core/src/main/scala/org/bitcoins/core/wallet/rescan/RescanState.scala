package org.bitcoins.core.wallet.rescan

sealed trait RescanState

object RescanState {

  /** Finished a rescan */
  case object RescanDone extends RescanState

  /** A rescan has already been started */
  case object RescanInProgress extends RescanState

}
