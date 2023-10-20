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

  /** For the case where we do not need to initiate a recursive rescan with a fresh pool of wallet addresses */
  case object RescanNotNeeded extends RescanState

  /** A rescan has already been started */
  case object RescanAlreadyStarted extends RescanState

  /** Indicates a rescan has bene started
    * The promise [[completeRescanEarlyP]] gives us the ability to terminate
    * the rescan early by completing the promise
    * [[blocksMatchedF]] is a future that is completed when the rescan is done
    * this returns all blocks that were matched during the rescan.
    * [[recursiveRescanP]] is a promise that is completed when there is a recursive rescan is started or there is not a recursive rescan started
    */
  case class RescanStarted(
      private val completeRescanEarlyP: Promise[Option[Int]],
      blocksMatchedF: Future[Vector[BlockMatchingResponse]],
      recursiveRescanP: Promise[RescanState])(implicit ec: ExecutionContext)
      extends RescanState {

    private val _isCompletedEarly: AtomicBoolean = new AtomicBoolean(false)
    //the promise returned by Source.maybe completes with None
    //if the stream terminated because the rescan was complete.
    completeRescanEarlyP.future.map { _ =>
      _isCompletedEarly.set(false)
    }

    completeRescanEarlyP.future.failed.foreach {
      case RescanTerminatedEarly =>
        recursiveRescanP.failure(RescanTerminatedEarly)
        _isCompletedEarly.set(true)
      case scala.util.control.NonFatal(_) => //do nothing
    }

    /** Useful for determining if the rescan was completed
      * externally by the promise to terminate the stream
      * or was completed because the rescan was fully executed
      */
    def isCompletedEarly: Boolean = _isCompletedEarly.get

    def isStopped: Boolean = entireRescanDoneF.isCompleted

    /** Means this single rescan is complete, but recursive rescans may not be completed */
    def singleRescanDoneF: Future[Vector[BlockMatchingResponse]] =
      blocksMatchedF

    /** Means the entire rescan is done (including recursive rescans). This future is completed
      * when we rescan filters with addresses do not contain funds within [[WalletAppConfig.addressGapLimit]]
      */
    def entireRescanDoneF: Future[Vector[BlockMatchingResponse]] = {
      for {
        b0 <- blocksMatchedF
        recursive <- recursiveRescanP.future
        b1 <- recursive match {
          case r: RescanStarted => r.blocksMatchedF
          case RescanDone | RescanAlreadyStarted | RescanNotNeeded =>
            Future.successful(Vector.empty)
        }
      } yield b0 ++ b1
    }

    /** Fails a rescan with the given exception */
    def fail(err: Throwable): Unit = {
      completeRescanEarlyP.failure(err)
      recursiveRescanP.failure(err)
    }

    /** Completes the stream that the rescan in progress uses.
      * This aborts the rescan early.
      */
    def stop(): Future[Vector[BlockMatchingResponse]] = {
      val f = if (!completeRescanEarlyP.isCompleted) {
        val stoppedRecursiveRescanF = recursiveRescanP.future.flatMap {
          case started: RescanStarted => started.stop()
          case RescanDone | RescanAlreadyStarted | RescanNotNeeded =>
            Future.unit
        }
        stoppedRecursiveRescanF.map(_ => fail(RescanTerminatedEarly))
      } else {
        Future.unit
      }
      f.flatMap { _ =>
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
  }

  def awaitSingleRescanDone(rescanState: RescanState)(implicit
      ec: ExecutionContext): Future[Unit] = {
    rescanState match {
      case RescanState.RescanDone | RescanState.RescanAlreadyStarted |
          RescanState.RescanNotNeeded =>
        Future.unit
      case started: RescanState.RescanStarted =>
        started.singleRescanDoneF.map(_ => ())
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
      case RescanState.RescanDone | RescanState.RescanAlreadyStarted |
          RescanState.RescanNotNeeded =>
        Future.unit
      case started: RescanState.RescanStarted =>
        started.entireRescanDoneF.map(_ => ())
    }
  }

  /** Returns a Future that is completed when a rescan is fully executed.
    * This means that the rescan was NOT terminated externally by completing
    * the akka stream that underlies the rescan logic.
    */
  def awaitRescanComplete(rescanState: RescanState)(implicit
      ec: ExecutionContext): Future[Unit] = {
    rescanState match {
      case RescanState.RescanDone | RescanState.RescanAlreadyStarted |
          RescanState.RescanNotNeeded =>
        Future.unit
      case started: RescanState.RescanStarted =>
        started.entireRescanDoneF.flatMap { _ =>
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
