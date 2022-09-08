package org.bitcoins.core.wallet.rescan

import org.bitcoins.core.api.wallet.NeutrinoWalletApi.BlockMatchingResponse

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{ExecutionContext, Future, Promise}

sealed trait RescanState

object RescanState {

  case object RescanTerminatedEarly
      extends RuntimeException(s"Rescan terminated early")

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
      blocksMatchedF: Future[Vector[BlockMatchingResponse]])(implicit
      ec: ExecutionContext)
      extends RescanState {

    private val _isCompletedEarly: AtomicBoolean = new AtomicBoolean(false)
    //the promise returned by Source.maybe completes with None
    //if the stream terminated because the rescan was complete.
    completeRescanEarlyP.future.map { _ =>
      _isCompletedEarly.set(false)
    }

    completeRescanEarlyP.future.failed.foreach { case RescanTerminatedEarly =>
      _isCompletedEarly.set(true)
    }

    /** Useful for determining if the rescan was completed
      * externally by the promise to terminate the stream
      * or was completed because the rescan was fully executed
      */
    def isCompletedEarly: Boolean = _isCompletedEarly.get

    def isStopped: Boolean = doneF.isCompleted

    def doneF: Future[Vector[BlockMatchingResponse]] = blocksMatchedF

    /** Fails a rescan with the given exception */
    def fail(err: Throwable): Unit = {
      completeRescanEarlyP.failure(err)
    }

    /** Completes the stream that the rescan in progress uses.
      * This aborts the rescan early.
      */
    def stop(): Future[Vector[BlockMatchingResponse]] = {
      if (!completeRescanEarlyP.isCompleted) {
        fail(RescanTerminatedEarly)
      }
      blocksMatchedF.recoverWith {
        case RescanTerminatedEarly =>
          //this means this was purposefully terminated early
          //don't propagate the exception
          Future.successful(Vector.empty)
        case err =>
          throw err
      }
    }
  }

  /** Returns a Future for all rescan states that will be complete when the rescan is done.
    * This can be because the stream was externally termianted early, or the rescan completes.
    * If you are interested in just the stream completing beacuse the rescan was fully executed,
    * use [[awaitComplete())]]
    */
  def awaitRescanDone(rescanState: RescanState)(implicit
      ec: ExecutionContext): Future[Unit] = {
    rescanState match {
      case RescanState.RescanDone | RescanState.RescanAlreadyStarted =>
        Future.unit
      case started: RescanState.RescanStarted =>
        started.doneF.map(_ => ())
    }
  }

  /** Returns a Future that is completed when a rescan is fully executed.
    * This means that the rescan was NOT terminated externally by completing
    * the akka stream that underlies the rescan logic.
    */
  def awaitRescanComplete(rescanState: RescanState)(implicit
      ec: ExecutionContext): Future[Unit] = {
    rescanState match {
      case RescanState.RescanDone | RescanState.RescanAlreadyStarted =>
        Future.unit
      case started: RescanState.RescanStarted =>
        started.doneF.flatMap { _ =>
          if (started.isCompletedEarly) {
            Future.failed(
              new RuntimeException(
                s"Rescan was completed early, so cannot fulfill this request"))
          } else {
            Future.unit
          }
        }
    }
  }

}
