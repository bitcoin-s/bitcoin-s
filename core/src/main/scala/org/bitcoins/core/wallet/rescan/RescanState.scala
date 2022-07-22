package org.bitcoins.core.wallet.rescan

import org.bitcoins.core.api.wallet.NeutrinoWalletApi.BlockMatchingResponse

import scala.concurrent.{Future, Promise}

sealed trait RescanState

object RescanState {

  /** Finished a rescan */
  case object RescanDone extends RescanState

  /** A rescan has already been started */
  case object RescanAlreadyStarted extends RescanState

  /** Indicates a rescan has bene started
    * The promise [[completeRescanEarlyP]] gives us the ability to terminate
    * the rescan early by completing the promise
    * [[blocksMatchedF]] is a future that is completed when the rescan is done
    * this returns all blocks that were matched during the rescan.
    */
  case class RescanStarted(
      private val completeRescanEarlyP: Promise[Option[Int]],
      blocksMatchedF: Future[Vector[BlockMatchingResponse]])
      extends RescanState {

    def isStopped: Boolean = doneF.isCompleted

    def doneF: Future[Vector[BlockMatchingResponse]] = blocksMatchedF

    /** Completes the stream that the rescan in progress uses.
      * This aborts the rescan early.
      */
    def stop(): Future[Vector[BlockMatchingResponse]] = {
      completeRescanEarlyP.success(None)
      blocksMatchedF
    }
  }

}
