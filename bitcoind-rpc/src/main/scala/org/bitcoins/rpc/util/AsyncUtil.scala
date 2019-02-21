package org.bitcoins.rpc.util

import akka.actor.ActorSystem
import org.bitcoins.core.util.BitcoinSLogger

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future, Promise}

abstract class AsyncUtil extends BitcoinSLogger {

  private def retryRunnable(
      condition: => Boolean,
      p: Promise[Boolean]): Runnable = new Runnable {
    override def run(): Unit = {
      p.success(condition)
      ()
    }
  }

  def retryUntilSatisfied(
      condition: => Boolean,
      duration: FiniteDuration,
      maxTries: Int = 50)(implicit system: ActorSystem): Future[Unit] = {
    val f = () => Future.successful(condition)
    retryUntilSatisfiedF(f, duration, maxTries)
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
      duration: FiniteDuration = 100.millis,
      maxTries: Int = 50)(implicit system: ActorSystem): Future[Unit] = {
    val stackTrace: Array[StackTraceElement] =
      Thread.currentThread().getStackTrace

    retryUntilSatisfiedWithCounter(conditionF = conditionF,
                                   duration = duration,
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
      duration: FiniteDuration,
      counter: Int = 0,
      maxTries: Int,
      stackTrace: Array[StackTraceElement])(
      implicit system: ActorSystem): Future[Unit] = {

    implicit val ec = system.dispatcher

    conditionF().flatMap { condition =>
      if (condition) {
        Future.successful(())
      } else if (counter == maxTries) {
        Future.failed(RpcRetryException("Condition timed out", stackTrace))
      } else {
        val p = Promise[Boolean]()
        val runnable = retryRunnable(condition, p)

        system.scheduler.scheduleOnce(duration, runnable)

        p.future.flatMap {
          case true => Future.successful(())
          case false =>
            retryUntilSatisfiedWithCounter(conditionF,
                                           duration,
                                           counter + 1,
                                           maxTries,
                                           stackTrace)
        }
      }
    }
  }

  /**
    * Blocks until condition becomes true, the condition
    * is checked maxTries times, or overallTimeout is reached
    * @param condition The blocking condition
    * @param duration The interval between calls to check condition
    * @param maxTries If condition is tried this many times, an exception is thrown
    * @param overallTimeout If this much time passes, an exception is thrown.
    *                       This exists in case calls to condition take significant time,
    *                       otherwise just use duration and maxTries to configure timeout.
    * @param system An ActorSystem to schedule calls to condition
    */
  def awaitCondition(
      condition: () => Boolean,
      duration: FiniteDuration = 100.milliseconds,
      maxTries: Int = 50,
      overallTimeout: FiniteDuration = 1.hour)(
      implicit system: ActorSystem): Unit = {

    //type hackery here to go from () => Boolean to () => Future[Boolean]
    //to make sure we re-evaluate every time retryUntilSatisfied is called
    def conditionDef: Boolean = condition()
    val conditionF: () => Future[Boolean] = () =>
      Future.successful(conditionDef)

    awaitConditionF(conditionF, duration, maxTries, overallTimeout)
  }

  def awaitConditionF(
      conditionF: () => Future[Boolean],
      duration: FiniteDuration = 100.milliseconds,
      maxTries: Int = 50,
      overallTimeout: FiniteDuration = 1.hour)(
      implicit system: ActorSystem): Unit = {

    val f: Future[Unit] = retryUntilSatisfiedF(conditionF = conditionF,
                                               duration = duration,
                                               maxTries = maxTries)

    Await.result(f, overallTimeout)
  }
}

object AsyncUtil extends AsyncUtil
