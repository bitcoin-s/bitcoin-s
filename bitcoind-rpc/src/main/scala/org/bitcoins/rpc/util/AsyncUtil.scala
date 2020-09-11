package org.bitcoins.rpc.util

import akka.actor.ActorSystem
import org.bitcoins.core.util.{BitcoinSLogger, FutureUtil}

import scala.concurrent._
import scala.concurrent.duration.{DurationInt, FiniteDuration}

abstract class AsyncUtil extends BitcoinSLogger {
  import AsyncUtil.DEFAULT_MAX_TRIES

  private def retryRunnable(
      condition: => Boolean,
      p: Promise[Boolean]): Runnable =
    new Runnable {

      override def run(): Unit = {
        p.success(condition)
        ()
      }
    }

  def retryUntilSatisfied(
      condition: => Boolean,
      interval: FiniteDuration = AsyncUtil.DEFAULT_INTERVAL,
      maxTries: Int = DEFAULT_MAX_TRIES)(implicit
      system: ActorSystem): Future[Unit] = {
    val f = () => Future.successful(condition)
    retryUntilSatisfiedF(f, interval, maxTries)
  }

  /**
    * The returned Future completes when condition becomes true
    * @param conditionF The condition being waited on
    * @param duration The interval between calls to check condition
    * @param maxTries If condition is tried this many times, the Future fails
    * @param system An ActorSystem to schedule calls to condition
    * @return A Future[Unit] that succeeds if condition becomes true and fails otherwise
    */
  def retryUntilSatisfiedF(
      conditionF: () => Future[Boolean],
      interval: FiniteDuration = AsyncUtil.DEFAULT_INTERVAL,
      maxTries: Int = DEFAULT_MAX_TRIES)(implicit
      system: ActorSystem): Future[Unit] = {
    val stackTrace: Array[StackTraceElement] =
      Thread.currentThread().getStackTrace

    retryUntilSatisfiedWithCounter(conditionF = conditionF,
                                   interval = interval,
                                   maxTries = maxTries,
                                   stackTrace = stackTrace)
  }

  case class RpcRetryException(
      message: String,
      caller: Array[StackTraceElement])
      extends Exception(message) {

    /*
    Someone who calls a method in this class will be interested
     * in where the call was made (and the stack trace from there
     * backwards) and what happens between their call and the failure,
     * i.e. the internal calls of this class, are not of interest.
     *
     * This trims the top of the stack trace to exclude these internal calls.
     */
    val internalFiles: Vector[String] = Vector("AsyncUtil.scala",
                                               "RpcUtil.scala",
                                               "TestAsyncUtil.scala",
                                               "TestRpcUtil.scala")

    private val relevantStackTrace =
      caller.tail.dropWhile(elem => internalFiles.contains(elem.getFileName))

    this.setStackTrace(relevantStackTrace)
  }

  // Has a different name so that default values are permitted
  protected def retryUntilSatisfiedWithCounter(
      conditionF: () => Future[Boolean],
      interval: FiniteDuration,
      counter: Int = 0,
      maxTries: Int,
      stackTrace: Array[StackTraceElement])(implicit
      system: ActorSystem): Future[Unit] = {

    import system.dispatcher

    conditionF().flatMap { condition =>
      if (condition) {
        FutureUtil.unit
      } else if (counter == maxTries) {
        Future.failed(RpcRetryException(
          s"Condition timed out after $maxTries attempts with interval=$interval waiting periods",
          stackTrace))
      } else {
        val p = Promise[Boolean]()
        val runnable = retryRunnable(condition, p)

        system.scheduler.scheduleOnce(delay = interval, runnable = runnable)

        p.future.flatMap {
          case true => FutureUtil.unit
          case false =>
            retryUntilSatisfiedWithCounter(conditionF = conditionF,
                                           interval = interval,
                                           counter = counter + 1,
                                           maxTries = maxTries,
                                           stackTrace = stackTrace)
        }
      }
    }
  }

  /**
    * Returns a future that resolved when the condition becomes true, the condition
    * is checked maxTries times, or overallTimeout is reached
    * @param condition The blocking condition
    * @param duration The interval between calls to check condition
    * @param maxTries If condition is tried this many times, an exception is thrown
    * @param system An ActorSystem to schedule calls to condition
    */
  def awaitCondition(
      condition: () => Boolean,
      interval: FiniteDuration = AsyncUtil.DEFAULT_INTERVAL,
      maxTries: Int = DEFAULT_MAX_TRIES)(implicit
      system: ActorSystem): Future[Unit] = {

    //type hackery here to go from () => Boolean to () => Future[Boolean]
    //to make sure we re-evaluate every time retryUntilSatisfied is called
    def conditionDef: Boolean = condition()
    val conditionF: () => Future[Boolean] = () =>
      Future.successful(conditionDef)

    awaitConditionF(conditionF, interval, maxTries)
  }

  def awaitConditionF(
      conditionF: () => Future[Boolean],
      interval: FiniteDuration = AsyncUtil.DEFAULT_INTERVAL,
      maxTries: Int = DEFAULT_MAX_TRIES)(implicit
      system: ActorSystem): Future[Unit] = {

    retryUntilSatisfiedF(conditionF = conditionF,
                         interval = interval,
                         maxTries = maxTries)

  }
}

object AsyncUtil extends AsyncUtil {

  /**
    * The default interval between async attempts
    */
  private[bitcoins] val DEFAULT_INTERVAL: FiniteDuration = 100.milliseconds

  /**
    * The default number of async attempts before timing out
    */
  private[bitcoins] val DEFAULT_MAX_TRIES: Int = 50

}
