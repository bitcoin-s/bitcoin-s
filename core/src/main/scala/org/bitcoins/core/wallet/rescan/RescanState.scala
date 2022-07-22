package org.bitcoins.core.wallet.rescan

import scala.concurrent.{Future, Promise}

sealed trait RescanState

object RescanState {

  /** Finished a rescan */
  case object RescanDone extends RescanState

  case object RescanAlreadyStarted extends RescanState

  /** A rescan has already been started */
  case class RescanStarted(private val completeRescanP: Promise[Option[Int]])
      extends RescanState {

    /** Completes the stream that the rescan in progress uses.
      * This aborts the rescan early.
      */
    def stop(): Future[Option[Int]] = {
      completeRescanP.success(None)
      completeRescanP.future
    }
  }

}
